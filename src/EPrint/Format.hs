{-# LANGUAGE PatternSynonyms, ApplicativeDo #-}
module EPrint.Format (
    -- document formatting type
    Doc,

    -- elementary documents and their combining operators
    empty, text,
    (<>), (<+>), ($$), (</>), (|:|), (|+|), (<|>),
    hcat, hsep, vcat, choose,
    wrap, wrapInd, wrapComma, wrapWith,

    -- helper documents
    space, tab,

    -- local options modification, resulting solution modification
    local, alter, incCost,
    toLeft, toCenter, toRight,

    -- format with shared elements
    Fmt(..), partial, isolated, runFmt,

    -- solve Doc format
    solveDoc
) where

import EPrint.Options
import EPrint.Layout
import EPrint.Solution
import Data.List
import Debug.Trace

-------------------------------------
-- Document representation & building operations
-------------------------------------

-- The Doc data represents tree of formating instructions and it can also represent partial or complete solution of format.
data Doc
    = Text    String
    | TConcat Doc Doc   -- tail concatenation (to last row of vertical concats)
    | HConcat Doc Doc   -- horizontal concatenation (making parallel columns of rows)
    | VConcat Doc Doc   -- vertical concatenation
    | Choice  Doc Doc   -- selects cheaper of two layouts
    | Alter (Options -> Doc -> Doc) -- postpone generation of Doc to format solving time (function takes options and suffix)
    | Complete Solution -- complete solution, all other documents are partial solutions
    | Deferred          -- deferred solution to be completed later by providing suffix

pattern Empty = Complete EmptySol

empty :: Doc
empty = Empty

text :: String -> Doc
text s = Text s

space = text " "
tab   = text "    "

-- combines two documents together by given constructor if they are not Empty or drops constructor and returns non-empty document
combine :: (Doc -> Doc -> Doc) -> Doc -> Doc -> Doc
combine _ Empty b = b
combine _ a Empty = a
combine x a b = a `x` b

(<>), (<+>), ($$), (</>), (|:|), (|+|), (<|>) :: Doc -> Doc -> Doc
infixl 6 <>, <+>
infixl 5 $$, </>
infixl 4 |:|, |+|
infixl 3 <|>

(<> ) = combine $ TConcat
(<+>) = combine $ \a b -> a <> space <> b
($$ ) = combine $ VConcat
(<|>) = combine $ Choice
(</>) = combine $ \a b -> runFmt $ do a' <- partial a
                                      b' <- partial b
                                      pure $ a' <+> b' <|> a' $$ b'
(|:|) = combine $ HConcat
(|+|) = combine $ \a b -> a |:| space <> b

hcat, hsep, vcat, choose :: [Doc] -> Doc
hcat   = foldr (<>)  empty
hsep   = foldr (<+>) empty
vcat   = foldr ($$)  empty
choose = foldr (<|>) empty

wrap, wrapInd, wrapComma :: [Doc] -> Doc
wrap l       = wrapWith l space Empty Empty
wrapInd l    = wrapWith l space Empty tab
wrapComma l  = wrapWith l (text ", ") (text ",") Empty

-- <+>, $$ - operators to combine elements horizontally and vertically
wrapWith :: [Doc] -> Doc -> Doc -> Doc -> Doc
wrapWith []  _ _ _ = Empty
wrapWith [e] _ _ _ = e
wrapWith elems sepH preV postV = Alter doWrap where
    doWrap opt suffix = head allMins where -- first choice of allMins is choice between all possible variants
        -- Take elements from first to last and make choice between variants of concatenated elements, preceded with solutions
        -- for previous rows, for increasing number of them. Thus incrementally building choice between all possible variants,
        -- which fit configured line width. Each choice c from allMins means:
        --     c[i] is minimum of [c[i-1] $$ elem[i], c[i-2] $$ elem[i-1] <+> elem[i], ...]

        es = map (\doc -> solve opt doc Empty) $ reverse elems
        allMins = step suffix es acc where -- step for last element carries suffix of wrapped elements
            acc = foldr (step preV) [Empty] $ tail (tails es)

        -- extend concats by elem and compute choice variants for extended prefix of wrapped elements
        step :: Doc -> [Doc] -> [Doc] -> [Doc]
        step sx es mins = m:mins where
            m = foldl' (variant sx) Empty $ zip mins cats
            cats = takeWhile fitsLine $ tail $ scanl' hcat Empty es where
                fitsLine (Complete sol) = layWidth (solLayout sol 0) <= lineWidth opt
                hcat cat elem = solve opt (elem <+> cat) Empty

        -- newMin  - accumulated choice between all formating variants of elements considered so far
        -- newCats - accumulated concats of elements - suffixed by newly considered element
        variant :: Doc -> Doc -> (Doc, Doc) -> Doc
        variant sx newMin (min, cat) = solve opt (newMin <|> (min $+ (cat <> sx))) Empty

        (<+>) = combine $ \a b -> a <> sepH <> b
        ($+)  = combine $ \a b -> VConcat a (postV <> b)

-- local modification of options (e.g. line-break or per-character cost)
local :: (Options -> Options) -> Doc -> Doc
local _ Empty = Empty
local f doc = Alter $ \opt suffix -> solve (f opt) doc suffix

-- alter cost of solution (to change preference of alternative variants)
alter :: (Solution -> Solution) -> Doc -> Doc
alter f Empty = Complete $ f EmptySol
alter f doc = Alter $ \opt suffix ->
    case solve opt doc suffix of Complete sol -> Complete $ f sol
                                 part         -> alter f part

incCost :: Double -> Doc -> Doc
incCost cost = alter (solIncCost cost)

toLeft, toCenter, toRight :: Doc -> Doc
toLeft   doc = alter (solJustify PLeft)   doc
toCenter doc = alter (solJustify PCenter) doc
toRight  doc = alter (solJustify PRight)  doc


-------------------------------------
-- Computing solution for given document representation
-------------------------------------
-- Always returns Complete solution if given complete suffix, partial solution otherwise.
solve :: Options -> Doc -> Doc -> Doc
solve opt doc suffix = case doc of -- suffix is introduced by tail concat
    Text s      -> prepH opt sol suffix where         -- note usage of prepH instead of prepT (they make no difference here),
                       sol = Complete $ solText opt s -- it allows later completition of (HConcat Complete Deferred) in Choice
    TConcat a b -> solve opt a $ solve opt b suffix
    HConcat a b -> prepH opt solA solB where
                       solA = solve opt a Empty
                       solB = solve opt b suffix
    VConcat a b -> prepV opt solA solB where
                       solA = solve opt a Empty
                       solB = solve opt b suffix
    Choice a b  -> case suffix of
                       Complete _ -> choice suffix
                       Deferred   -> choice Deferred
                       _          -> case choice Deferred of sol@(Complete _) -> prepH opt sol suffix
                                                             psol             -> TConcat psol suffix
                       where choice sx = prepChoice opt solA solB where solA = undefer $ solve opt a sx
                                                                        solB = undefer $ solve opt b sx
    Alter f     -> f opt suffix
    Complete _  -> prepH opt doc suffix -- note usage of prepH instead of prepT (they make no difference here)
    Deferred    -> suffix
    where prepH, prepV, prepChoice :: Options -> Doc -> Doc -> Doc
          prepH opt (Complete s1) (Complete s2) = Complete $ solCatH opt s1 s2
          prepH opt (Complete s1) (HConcat (Complete s2) ps) = HConcat (Complete (solCatH opt s1 s2)) ps
          prepH _   p1 p2 = HConcat p1 p2
          
          prepV opt (Complete s1) (Complete s2) = Complete $ solSumV opt s1 s2
          prepV opt (Complete s1) (VConcat (Complete s2) ps) = VConcat (Complete (solSumV opt s1 s2)) ps
          prepV _   p1 p2 = VConcat p1 p2
          
          prepChoice opt (Complete s1) (Complete s2)             = Complete $ solMin opt s1 s2
          prepChoice opt (Complete s1) (Choice (Complete s2) ps) = Choice (Complete (solMin opt s1 s2)) ps
          prepChoice _   p1 p2 = Choice p1 p2
          
          -- removes needless Deferred from partial solution thus making it complete (but leaves it where it makes difference)
          undefer Deferred = Empty
          undefer (HConcat c@(Complete _) Deferred) = c
          undefer p = p


solveDoc :: Options -> Doc -> Solution
solveDoc opt doc = sol where Complete sol = solve opt doc Empty


-------------------------------------
-- Building Doc format with shared elements (to avoid excessive computational complexity)
-------------------------------------
-- See </> operator for usage example.
newtype Fmt a = F { unF :: Options -> a }

instance Functor Fmt where
    fmap f (F fmt) = F $ \o -> f $ fmt o

instance Applicative Fmt where
    pure v = F $ \_ -> v
    F f1 <*> F f2 = F $ \o -> f1 o (f2 o)

-- make partial solution by usage of deferred suffix - it is completed later by providing actual suffix
partial :: Doc -> Fmt Doc
partial doc = F $ \o -> solve o doc Deferred

-- use carefully - given Doc must be independent of suffix - always return Complete
isolated :: Doc -> Fmt Doc
isolated doc = F $ \o -> solve o doc Empty

runFmt :: Fmt Doc -> Doc
runFmt (F fmt) = Alter $ \opt suffix -> solve opt (fmt opt) suffix


-------------------------------------
-- Debugging utilities
-------------------------------------
instance Show Doc where show doc = showDoc Empty doc
showDoc :: Doc -> Doc -> String
showDoc _                 (Text s)      = s
showDoc _             doc@(TConcat a b) = showDoc doc a ++ " + " ++ showDoc doc b
showDoc _             doc@(HConcat a b) = showDoc doc a ++ " ++ " ++ showDoc doc b
showDoc (TConcat _ _) doc@(VConcat _ _) = "(" ++ showDoc Empty doc ++ ")"
showDoc (HConcat _ _) doc@(VConcat _ _) = "(" ++ showDoc Empty doc ++ ")"
showDoc _             doc@(VConcat a b) = showDoc doc a ++ " $ " ++ showDoc doc b
showDoc (TConcat _ _) doc@(Choice  _ _) = "(" ++ showDoc Empty doc ++ ")"
showDoc (HConcat _ _) doc@(Choice  _ _) = "(" ++ showDoc Empty doc ++ ")"
showDoc (VConcat _ _) doc@(Choice  _ _) = "(" ++ showDoc Empty doc ++ ")"
showDoc _             doc@(Choice  a b) = showDoc doc a ++ " | " ++ showDoc doc b
showDoc _             doc@(Alter f)     = "Alter(...)"
showDoc _                 (Complete _)  = "@"
showDoc _                 Deferred      = "!"

