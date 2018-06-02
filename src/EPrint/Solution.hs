{-# LANGUAGE PatternSynonyms #-}
module EPrint.Solution (
        -- solution type
        Solution, pattern EmptySol,

        -- creation and combination of solutions
        solText, solCatH, solSumV, solMin,
        solIncCost, solJustify,

        -- solution properties
        solMinWidth, solMaxWidth, solLayout,
) where

import EPrint.Options
import EPrint.AText
import EPrint.Layout
import Numeric (showFFloat)
import Data.List
import Data.List.Zipper
import Data.Tuple (swap)
import GHC.Exts (groupWith, sortWith)
import Debug.Trace

-- floating point numbers approximation comparison
apxEps = 1e-5
apxEq, apxNonEq, apxLess, apxLessEq, apxGreater, apxGreaterEq :: Double -> Double -> Bool
apxEq        a b = abs (a-b) <= apxEps
apxNonEq     a b = abs (a-b) >  apxEps
apxLess      a b = a <  b - apxEps
apxLessEq    a b = a <= b + apxEps
apxGreater   a b = a >  b + apxEps
apxGreaterEq a b = a >= b - apxEps
apxZero, apxNonZero :: Double -> Bool
apxZero    a = abs a <= apxEps
apxNonZero a = abs a >  apxEps

-- take elements while they satisfy given predicate plus first element which does not satisfy it
takeWhilePlus :: (a -> Bool) -> [a] -> [a]
takeWhilePlus pred (e:es) | pred e    = e : takeWhilePlus pred es
                          | otherwise = [e] -- plus first element which does not satisfy predicate
takeWhilePlus _ _ = []

-- The Solution type represents optimal layout of source format given in Doc tree for arbitrary target column width
-- up to lineWidth property of Options. It is based on technique described in "A New Approach to Optimal Code Formatting"
-- by Phillip M. Yelland, but it uses slightly different representation. Instead of knots delimiting line segments of print
-- starting positions (and holding layout/cost for each segment), here knots delimit ranges of target column width, from
-- the narrowest width to the widest one, where layout/cost of knot spans from previous knot in solution (zero width if first)
-- to its width property or in case of last knot also for any wider targer column.
newtype Solution = Solution [Knot]
    deriving (Eq, Show)

pattern EmptySol = Solution []

solMinWidth EmptySol         = 0
solMinWidth (Solution (k:_)) = width k
solMaxWidth EmptySol         = 0
solMaxWidth (Solution ks)    = width (last ks)
solLayout   EmptySol      _  = layEmpty
solLayout   (Solution ks) w  = layout (knotAt ks w)

knotAt [k]    _ = k
knotAt (k:ks) w | width k < w = knotAt ks w
                | otherwise   = k

-- Knots represent different layout variants for different target column widths. Some knots share layout but account for
-- different cost gradient.
data Knot = Knot {
    width     :: !Int,       -- width of column
    layout    :: !Layout,    -- optimal layout for target column width range represented by this segment
    intercept :: !Double,    -- fixed cost of layout for width range of this segment
    gradient  :: !Double     -- cost increase per character beyond target range
}

showF f = showFFloat (Just 2) f ""

instance Eq Knot where
    a == b = width a == width b && intercept a == intercept b && gradient a == gradient b

instance Show Knot where
    show k = "(" ++ show (width k) ++ ":"  ++ showF (intercept k) ++ "/" ++ showF (gradient k) ++ "; " ++ show (layout k) ++ ")"

costAt, gradAt :: Knot -> Int -> Double
costAt k w | w < width k = intercept k + (fromIntegral $ width k - w) * gradient k
           | otherwise   = intercept k

gradAt k w | w <= width k = gradient k
gradAt _ _ = 0

-- prepend knot to solution in construction or drop it if it is linear interpolation of first one in list
prepend n (k:ks) | gradient n `apxEq` gradient k && intercept n `apxEq` costAt k (width n) = k:ks
prepend n ks = n:ks


solIncCost :: Double -> Solution -> Solution
solIncCost costInc (Solution ks) = Solution $ map (\k -> k { intercept = intercept k + costInc }) ks

solJustify :: Position -> Solution -> Solution
solJustify pos (Solution ks) = Solution $ map (\k -> k { layout = layJustify pos (layout k) }) ks

solText :: Options -> AText -> Solution
solText opt s | tlLength s <= lineWidth opt = Solution [Knot    (tlLength s) (layText s) intercept     (costLeftOver opt)]
              | otherwise                   = Solution [Knot (lineWidth opt) (layText s) interceptOver (costLeftOver opt)]
              where intercept = fromIntegral (tlLength s) * costChar opt
                    leftOver = tlLength s - lineWidth opt
                    interceptOver = intercept + fromIntegral leftOver * costLeftOver opt

-- There are three versions of horizontal concatenation implemented: solCatHSingle, solCatHMulti, solCatHAll
-- The only one which should give you optimal layout in all cases is solCatHAll (modulo bugs), but it is also the least
-- effecient one. The solCatHSingle and solCatHMulti use heuristics to give you optimal layout in most cases with better
-- performance.
-- To have some idea, solCatHSingle should be good enough if you use only tail concatenation (but not |:|, |+|). But if you
-- use parallel columns of wrapped words, then solCatHMulti with right hcatCostVariance option shall be preferred and
-- in my experience its good enough.
-- Also note that while solCatHMulti and solCatHAll may look very complex, they work with very simple solutions (one knot only)
-- most of the time.
solCatH opt s1 s2 = solCatHMulti opt s1 s2

-- Retreating on zipper position towards its beginning to knot with smallest width bigger then given w.
-- Note that it counts on w being small enough, so that it do not have to check endp if calling right.
retreet :: Zipper Knot -> Int -> Zipper Knot
retreet z w | width (cursor z) <= w = right z
            | w < width (cursor z) && not (beginp z) = retreet (left z) w -- w may be negative
            | otherwise = z

-- concatenate two solutions into horizontal composition
solCatHSingle :: Options -> Solution -> Solution -> Solution
solCatHSingle _ sol1 (Solution []) = sol1
solCatHSingle _ (Solution []) sol2 = sol2
solCatHSingle opt (Solution ks1) (Solution ks2) = Solution $ kns ks1 ks2 where
    kns (k1:_) (k2:_) -- halfK is knot at position of k1, if k1 has different gradient than k2
        | gradient k1 `apxNonEq` gradient k2 = halfK : kcat (fromList ks1) (fromList ks2)
        | otherwise                          =         kcat (fromList ks1) (fromList ks2)
        where halfK = Knot (width k1) (layout k1 `layH` layout k2) (intercept k1 + costAt k2 0 - sharedCost k1 k2) (gradient k1)

    -- evaluate cost of combining layouts represented by given knots
    eval k1 k2 = intercept k1 + costAt k2 (lineWidth opt - width k1) - sharedCost k1 k2
    -- sharedCost is meant to eliminate multiplying of line break cost if concatenating vertical layouts
    sharedCost k1 k2 = fromIntegral ((layHeight (layout k1) `min` layHeight (layout k2)) - 1) * costLineBreak opt

    -- Knots are alternative layout/cost variants for width ranges they delimit. Horizontal concatenation then consists of all
    -- combinations of variants from source solutions, except these which are shadowed by some cheaper variant.
    kcat :: Zipper Knot -> Zipper Knot -> [Knot]
    kcat z1 z2 = currK `prepend` nextK (right z1) (right z2) where
        (k1, k2, catWidth) = (cursor z1, cursor z2, width k1 + width k2)
        currK = Knot (catWidth `min` lineWidth opt) (layout k1 `layH` layout k2) (eval k1 k2) grad
            where grad = if width k1 == lineWidth opt then gradient k1 else gradient k2
        -- Advances to next target width. Selects source variants with smallest concatenated width, bigger than last target
        -- width and prefer cheaper combination if they are equaly wide, to avoid combinations which are shadowed.
        nextK r1 r2
            | (endp r1 && endp r2) || catWidth >= lineWidth opt = [] -- no more knots or all other variants are too wide
            | endp r1 = next2
            | endp r2 = next1
            | width r1c + width l2c < width l1c + width r2c = next1
            | width r1c + width l2c > width l1c + width r2c = next2
            | eval r1c l2c < eval l1c r2c  = next1
            | eval r1c l2c > eval l1c r2c  = next2
            | gradient l2c <= gradient r2c = next1
            | otherwise                    = next2
            where (r1c, r2c, l1c, l2c) = (cursor r1, cursor r2, cursor l1, cursor l2)
                  next1 = kcat r1 l2
                  next2 = kcat l1 r2
                  -- advencing on one solution may need retreet on other to get smallest combination bigger than last one
                  l1 = z1 `retreet` (catWidth - width r2c)
                  l2 = z2 `retreet` (catWidth - width r1c)


-- concatenate two solutions into horizontal composition
solCatHMulti :: Options -> Solution -> Solution -> Solution
solCatHMulti _ sol1 (Solution []) = sol1
solCatHMulti _ (Solution []) sol2 = sol2
solCatHMulti opt (Solution ks1) (Solution ks2) = Solution $ kns ks1 ks2 where
    kns (k1:_) (k2:_) -- halfK is knot at position of k1, if k1 has different gradient than k2
        | gradient k1 `apxNonEq` gradient k2 = halfK : kcat k1 k2 [(fromList ks1, fromList ks2)]
        | otherwise                          =         kcat k1 k2 [(fromList ks1, fromList ks2)]
        where halfK = Knot (width k1) (layout k1 `layH` layout k2) (intercept k1 + costAt k2 0 - sharedCost k1 k2) (gradient k1)

    -- evaluate cost of combining layouts represented by given knots
    eval k1 k2 = intercept k1 + costAt k2 (lineWidth opt - width k1) - sharedCost k1 k2
    -- sharedCost is meant to eliminate multiplying of line break cost if concatenating vertical layouts
    sharedCost k1 k2 = fromIntegral ((layHeight (layout k1) `min` layHeight (layout k2)) - 1) * costLineBreak opt
    wc = width . cursor
    gc = gradient . cursor

    -- Knots are alternative layout/cost variants for width ranges they delimit. Horizontal concatenation then consists of all
    -- combinations of variants from source solutions, except these which are shadowed by some cheaper variant.
    kcat :: Knot -> Knot -> [(Zipper Knot, Zipper Knot)] -> [Knot]
    kcat k1 k2 zs = currK `prepend` nextK where
        catWidth = width k1 + width k2
        currK = Knot (catWidth `min` lineWidth opt) (layout k1 `layH` layout k2) (eval k1 k2) grad
            where grad = if width k1 == lineWidth opt then gradient k1 else gradient k2

        -- Advances to next target width. Selects source variants with smallest concatenated width, bigger than last target
        -- width and prefer cheaper combination if they are equaly wide, to avoid combinations which are shadowed.
        nextK | null nextPairs = []
              | otherwise = kcat (cursor min1) (cursor min2) cheapPairs
              where cheapPairs = map fst $ filter (\(_, e) -> e <= hcatCostVariance opt * minCost) relevantPairs
                    ((min1, min2), minCost) = foldr1 minCostPair relevantPairs
                    relevantPairs = nubBy eqPair $ concat $ map lessOrEqCost pairsWithCost
                    pairsWithCost = map (map (\p@(a, b) -> (p, eval (cursor a) (cursor b)))) nextPairs
                    lessOrEqCost (v1@(_, e1):rest) = v1 : takeWhile (\(_, e) -> e <= e1) rest
                    eqPair ((a, b), _) ((c, d), _) = wc a == wc c && wc b == wc d
                    minCostPair (p1@(a1, a2), e1) (p2@(b1, b2), e2)
                        | e1 < e2 = (p1, e1)
                        | e1 > e2 = (p2, e2)
                        | gc a2 < gc b2 = (p1, e1)
                        | gc a2 > gc b2 = (p2, e1)
                        | abs (wc a1 - wc a2) <= abs (wc b1 - wc b2) = (p1, e1)
                        | otherwise                                  = (p2, e1)

        -- generates list of position pairs which represent next step, they will be all given to next kcat,
        -- the knots pair with smallest cost will be given to kcat separately to create next concat-knot
        nextPairs :: [[(Zipper Knot, Zipper Knot)]]
        nextPairs = filter (not . null) $ map (takeWhile (\(a, b) -> wc a + wc b <= minWidth)) wzs where
            (wzs, minWidth) = foldr follow ([], maxBound) zs
            follow (z1, z2) (acc, minW)
                | (null n1 && null n2) || catWidth >= lineWidth opt = (acc, minW)
                | null n1 = (n2 : acc, wd n2 `min` minW)
                | null n2 = (n1 : acc, wd n1 `min` minW)
                | otherwise = (n1 : n2 : acc, wd n1 `min` wd n2 `min` minW)
                where (n1, n2) = (next z1 z2, map swap $ next z2 z1)
                      wd ((a, b):_) = wc a + wc b
                      next r l | endp rr   = []
                               | otherwise = (rr, ll) : next rr ll
                               where (rr, ll) = (right r, l `retreet` (catWidth - wc rr))


-- concatenate two solutions into horizontal composition
solCatHAll :: Options -> Solution -> Solution -> Solution
solCatHAll _ sol1 (Solution []) = sol1
solCatHAll _ (Solution []) sol2 = sol2
solCatHAll opt (Solution ks1) (Solution ks2) = sol where
    sol = Solution $ noLerp $ halfK (head ks1) (head ks2) : cheapKns
    cheapKns = map (foldr1 minCost) $ groupWith width $ sortWith width kns
    kns = [ Knot w (layout k1 `layH` layout k2) (eval k1 k2) grad |
            k1 <- ks1,
            k2 <- takeWhilePlus (\k -> width k1 + width k <= lineWidth opt) ks2,
            let w = lineWidth opt `min` (width k1 + width k2),
            let grad = if width k1 == lineWidth opt then gradient k1 else gradient k2 ]
    halfK k1 k2 = Knot (width k1) (layout k1 `layH` layout k2) (intercept k1 + costAt k2 0 - sharedCost k1 k2) (gradient k1)

    -- evaluate cost of combining layouts represented by given knots
    eval k1 k2 = intercept k1 + costAt k2 (lineWidth opt - width k1) - sharedCost k1 k2
    -- sharedCost is meant to eliminate multiplying of line break cost if concatenating vertical layouts
    sharedCost k1 k2 = fromIntegral ((layHeight (layout k1) `min` layHeight (layout k2)) - 1) * costLineBreak opt

    minCost k1 k2
        | intercept k1 < intercept k2 = k1
        | intercept k1 > intercept k2 = k2
        | gradient k1 <= gradient k2  = k1
        | otherwise                   = k2

    -- remove linear interpolations from solution
    noLerp (k:n:ks) | gradient k `apxEq` gradient n && intercept k `apxEq` costAt n (width k) = noLerp (n:ks)
                    | otherwise = k : noLerp (n:ks)
    noLerp k = k


-- vertical composition of two solutions
solSumV :: Options -> Solution -> Solution -> Solution
solSumV _ sol1 (Solution []) = sol1
solSumV _ (Solution []) sol2 = sol2
solSumV opt (Solution s1ks) (Solution s2ks) = Solution $ ksum s1ks s2ks where
    -- iterate through pairs of knots which are relevant to each width range and combine them together
    ksum ks1@(k1:ns1) ks2@(k2:ns2) = currK `prepend` nextK where
        currK = knot k1 k2 (width k1 `min` width k2) (gradient k1 + gradient k2)
        nextK | width k1 < width k2 = case ns1 of [] -> foldr next2 [] ks2
                                                  _  -> ksum ns1 ks2
              | width k1 > width k2 = case ns2 of [] -> foldr next1 [] ks1
                                                  _  -> ksum ks1 ns2
              | otherwise           = case (ns1, ns2) of ([],  _) -> foldr next2 [] ns2
                                                         ( _, []) -> foldr next1 [] ns1
                                                         _        -> ksum ns1 ns2 -- ksum ks1 ns2
        next1 n1 ks = knot n1 k2 (width n1) (gradient n1) `prepend` ks
        next2 n2 ks = knot k1 n2 (width n2) (gradient n2) `prepend` ks
    knot k1 k2 w grad = Knot w (layout k1 `layV` layout k2) (costAt k1 w + costAt k2 w + costLineBreak opt) grad


-- per width range minimum of two solutions
solMin :: Options -> Solution -> Solution -> Solution
solMin _ sol1 (Solution []) = sol1
solMin _ (Solution []) sol2 = sol2
solMin opt (Solution s1ks) (Solution s2ks) = Solution $ kns s1ks s2ks 0 where
    kns ks1@(k1:_) ks2@(k2:_) ws = kmin k1 k2 ws w $ next ks1 ks2 (w + 1) where w = width k1 `min` width k2

    -- given lists starting with last konts and next lower boundary of width range (ws), selects knot which will define next
    -- upper boundary and call kmin for selected range and knots relevant for that range
    next ks1@(k1:ns1) ks2@(k2:ns2) ws
        | width k1 < width k2 = nkns ns1 ks2 ws
        | width k1 > width k2 = nkns ks1 ns2 ws
        | otherwise           = nkns ns1 ns2 ws
        where nkns ls1@(_:_) ls2@(_:_) ws = kns ls1 ls2 ws
              nkns ls1       (l2:ms2)  ws = kmin k1 l2 ws w $ nkns ls1 ms2 (w + 1) where w = width l2
              nkns (l1:ms1)  ls2       ws = kmin l1 k2 ws w $ nkns ms1 ls2 (w + 1) where w = width l1
              nkns _         _         _  = []

    -- computes minimum of considered width range and prepends it to solution for following width ranges
    kmin k1 k2 ws w knsNext -- ws/w - lower/upper boundary of considered width range
        | i1 `apxLessEq` i2 = nextK k1 k2 i1 i2
        | otherwise         = nextK k2 k1 i2 i1
        where (i1, i2) = (costAt k1 w, costAt k2 w)
              -- creates knot from lower cost variant and checks for cost cross between last knot and current one, due to higher
              -- gradient of lower cost variant or irrelevant gradient of higher cost variant if we surpassed its width
              nextK kLow kHigh iLow iHigh -- low/high cost variant's knots and costs for width upper boundary
                  | hasCross && cross == w = knCross (w - 1) : (kn `prepend` knsNext)
                  | hasCross               = knCross  cross  : (kn `prepend` knsNext)
                  | otherwise              = kn `prepend` knsNext
                  where hasCross   = ws /= w && costAt kLow ws `apxGreater` costAt kHigh ws
                        kn         = Knot w (layout kLow)  iLow             (gradAt kLow  w)
                        knCross w  = Knot w (layout kHigh) (costAt kHigh w) (gradAt kHigh w)
                        cross      = w - ceiling ((iHigh - iLow) / (gradient kLow - gradAt kHigh w))

    -- prepend knot to solution in construction or drop it if it is linear interpolation of first one in list
    prepend n [k]    | apxZero (gradient k) && intercept n `apxEq` intercept k = [n]
    prepend n (k:ks) | gradient n `apxEq` gradient k && intercept n `apxEq` costAt k (width n) = k:ks
    prepend n ks = n:ks

