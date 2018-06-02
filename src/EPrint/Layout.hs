module EPrint.Layout (
    -- layout type
    Layout, Position(..),

    -- layout building elements
    layEmpty, layText, layH, layV, layJustify,

    -- layout properties
    layWidth, layHeight,

    -- layout printing
    layToATexts
) where

import EPrint.AText

-- Optimal layout ready to be printed
data Layout -- the Int numbers are width and height
    = LGap  !Int  -- white space to separate layout elements or empty branch of layout tree
    | LText !Int AText
    | LHCat !Int !Int Layout Layout
    | LVCat !Int !Int Layout Layout
    | LJustify !Position Layout

data Position = PLeft | PCenter | PRight
    deriving Show

layWidth (LGap  w)       = w
layWidth (LText w _)     = w
layWidth (LHCat w _ _ _) = w
layWidth (LVCat w _ _ _) = w
layWidth (LJustify _ l)  = layWidth l

layHeight (LGap  _)       = 1
layHeight (LText _ _)     = 1
layHeight (LHCat _ h _ _) = h
layHeight (LVCat _ h _ _) = h
layHeight (LJustify _ l)  = layHeight l

layEmpty = LGap 0

layText :: AText -> Layout
layText s = LText (tlLength s) s

layH, layV :: Layout -> Layout -> Layout
layH (LGap i) (LGap j) = LGap (i + j)
layH (LGap i) b        | i == 0 = b
layH a        (LGap j) | j == 0 = a
layH (LGap i) (LHCat w h (LGap j) b) = LHCat (w+i) h (LGap (j+i)) b
layH (LHCat w h a (LGap i)) (LGap j) = LHCat (w+j) h a (LGap (i+j))
layH a b = LHCat (layWidth a + layWidth b) (layHeight a `max` layHeight b) a b

layV (LGap i) (LGap j) = LGap (i `max` j)
layV (LGap i) b        = b
layV a        (LGap j) = a
layV a b = LVCat (layWidth a `max` layWidth b) (layHeight a + layHeight b) a b

layJustify :: Position -> Layout -> Layout
layJustify _ l@(LJustify _ _) = l
layJustify _ l@(LGap _)       = l
layJustify p l                = LJustify p l


layToATexts :: Layout -> [[AText]]
layToATexts l = loop l where
    loop l = let (hd, tl) = laySplit l
                 line = print hd [] in
             case tl of LGap _ -> [line]
                        _      -> line : loop tl

    print :: Layout -> [AText] -> [AText]
    print (LGap  w)       []  = []
    print (LGap  w)       acc = AText (replicate w ' ') : acc
    print (LText _ s)     acc = s : acc
    print (LHCat _ _ a b) acc = print a $ print b acc
    print (LJustify _ l)  acc = print l acc

-- split layout into head line and tail lines, tail is LGap if there is nothing left
laySplit :: Layout -> (Layout, Layout)
laySplit l = split l PLeft where
    split l j = case l of
        LGap  _     -> (l, l)
        LText w _   -> (l, LGap w)
        LHCat _ _ a b -> let (h1, t1) = split a PLeft
                             (h2, t2) = split b PLeft
                         in (h1 `layH` h2, t1 `layH` t2)
        LVCat w _ a b -> let (LVCat _ _ h t) = vHead a b
                             (hh, ht) = split h j
                         in (justify j hh w, justify j (ht `layV` t) w)
        LJustify p l  -> let (h, t) = split l p
                         in (layJustify p h, layJustify p t)
    
    -- restructure vertical block so that returned LVCat root of block is left/head-most element
    vHead (LVCat _ _ h t) b = vHead h $ t `layV` b
    vHead a b = a `layV` b

    justify _ (LJustify p l)  w = justify p l w
    justify p (LVCat _ h a b) w = LJustify p $ LVCat w h a b
    justify PLeft   l w = l `layH` LGap (w - layWidth l)
    justify PCenter l w = LGap (r `div` 2) `layH` l `layH` LGap (r - r `div` 2) where r = w - layWidth l
    justify PRight  l w = LGap (w - layWidth l) `layH` l


-------------------------------------
-- Debugging utilities
-------------------------------------
instance Show Layout where show = showLayout
showLayout :: Layout -> String
showLayout l = print (LGap 0) l where
    print _                 (LGap w)    = replicate w '_'
    print _                 (LText _ s) = tl2str s
    print _               n@(LHCat _ _ a b) = print n a ++ "+" ++ print n b
    print (LHCat _ _ _ _) n@(LVCat _ _ _ _) = "(" ++ print n n ++ ")"
    print _               n@(LVCat w _ a b) = print n a ++ "$" ++ print n b
    print _               n@(LJustify PLeft   l) = "LJ(" ++ print n l ++ ")"
    print _               n@(LJustify PCenter l) = "CJ(" ++ print n l ++ ")"
    print _               n@(LJustify PRight  l) = "RJ(" ++ print n l ++ ")"

