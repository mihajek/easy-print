{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import EPrint
import Data.List
import Debug.Trace
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as TS

main = do
    examples

test1 :: IO ()
test1 = do
    pprintIO defaultFmt $ text "x" <> text "y" <+> (text "z" $$
                                                    text "next" <+> text "..." $$
                                                    text "last") <+> text "end"
    pprintIO defaultFmt $
        (text "cau" <+> toRight (text "abcd" $$ text "a" $$ text "bc") $$
         text "nazdar" $$
         text "ahoj") |+|
        (text "x" $$ text "y" $$ text "z" $$ text "u" $$ text "v" $$ text "w")
    --pprintIO defaultFmt $ text "x" </> text "y"
    --pprintIO defaultFmt $ do wtp <- empty
    --                         local (\opt -> opt { lineWidth = 7, wrapTailPrefix = wtp }) $
    --                            wrap [text "a", text "b", text "c", text "d", text "e", text "f"]
    let fmt = defaultFmt { lineWidth = 12 }
    --pprintIO fmt $ wrap [text "a", text "b", text "c", text "d", text "e", text "f"]
    --pprintIO fmt $ wrap [text (show x) | x <- [1..100000]]
    --pprintIO fmt{lineWidth=3} $ wrap [text (show x) | x <- [1..20]]
    pprintIO fmt $ wrap [text (show x) | x <- [1..20]] |+| wrap [text (show x) | x <- [1..20]]
    pprintIO fmt $ (text "blabla" $$ text "cat") <+> wrap [text (show x) | x <- [1..20]]
    --pprintIO fmt $ wrap [text (show x) | x <- [1..20]] |+| (text "blabla" $$ text "cat")
    pprintIO fmt{lineWidth=12} $ wrap [text (show x) | x <- [1..3]] |:| wrap [text (show x) | x <- [1..15]]
    pprintIO fmt{lineWidth=9}  $ wrap [text (show x) | x <- [1..3]] |:| wrap [text (show x) | x <- [1..7]]

    --pprintIO defaultFmt{lineWidth=5} $ wrap [text (show x) | x <- [1..5]]

    let fmt = defaultFmt { lineWidth = 12 }
    pprintIO fmt $ wrap [text (show x) | x <- [1..20]] <+> (text "blabla" $$ text "cat")

test2 :: IO ()
test2 = do
    let fmt = defaultFmt { lineWidth = 12 }
    pprintIO fmt{lineWidth=50} $ wrap [text (show x) | x <- [1..10000]]
    pprintIO fmt $ wrap [text (show x) | x <- [1..1000]] </> text "nazdar"
    pprintIO fmt $ runFmt $ do w <- partial $ wrap [text (show x) | x <- [1..98]]
                               pure $ w <+> text "X" $$ w <+> text "nazdar" $$ (w |+| text "Y")

    pprintIO defaultFmt $ runFmt $ do sh1 <- partial $ text "a" <+> text "b"
                                      sh2 <- partial $ text "1" <+> text "2" <+> text "3"
                                      pure $ text "x" <+> sh1 <+> sh2 <|> sh1 <+> text "y" <+> sh2
    pprintIO defaultFmt $ let sh1 = text "a" <+> text "b"
                              sh2 = text "1" <+> text "2" <+> text "3"
                          in text "x" <+> sh1 <+> sh2 <|> sh1 <+> text "y" <+> sh2

    pprintIO defaultFmt $ runFmt $ do sh1 <- partial $ text "a" <+> text "b"
                                      sh2 <- partial $ wrap [text "1", text "2", text "3"]
                                      pure $ text "x" <+> sh1 <+> sh2 <|> sh1 <+> text "y" <+> sh2
    pprintIO defaultFmt $ wrap [text "1", text "2", text "3"]

test3 :: IO ()
test3 = do
    let fmt = defaultFmt { lineWidth = 12 }
    pprintIO fmt $ text (TS.pack "hello") <+> text (BS.pack "hi") <+> text "bye"

examples :: IO ()
examples = do
    let head w = putStrLn $ "|" ++ replicate (w-2) '-' ++ "|"
    let pw w doc = do head w
                      pprintIO defaultFmt { lineWidth = w } doc
    let pp = pw 80

    putStrLn "tail concatenation:"
    pp $ text "a" <> text "b"
    pp $ text "a" <+> text "b"
    putStrLn "vertical concatenation:"
    pp $ text "a" $$ text "b"

    putStrLn "combination:"
    pp $ text "a" <+> text "1" $$
         text "b" <+> text "2"
    putStrLn "'tail' goes to last row of vertical layout:"
    pp $ (text "a" $$
          text "b") <+> text "tail"
    putStrLn "vertical layout as tail of horizontal one:"
    pp $ text "a" <+> (text "1" $$
                       text "2")

    putStrLn "choose cheaper variant:"
    pw 10 $ text "longer-than-10" <|> text "short"
    putStrLn "prefer first if same expensive:"
    pp $ text "one" <|> text "two"

    putStrLn "choose between horizontal and vertical layout:"
    pp   $ text "one" </> text "two"
    pw 5 $ text "one" </> text "two"

    putStrLn "parallel columns:"
    pp $ (text "a" $$ text "b" $$ text "c") |+| (text "1" $$ text "2")
    pp $ (text "a" $$ text "b") |+| (text "1" $$ text "2" $$ text "3")

    putStrLn "columns justification:"
    pp $ toLeft   (text "one-two-three" $$ text "one-two" $$ text "one") |+|
         toCenter (text "one-two-three" $$ text "one-two" $$ text "one") |+|
         toRight  (text "one-two-three" $$ text "one-two" $$ text "one")
    putStrLn "single row justification:"
    pp $ text "one-two" $$ toCenter (text "one") $$ toRight (text "two")

    putStrLn "wrap:"
    pw 12 $ wrap      [text (show x) | x <- [1..20]]
    pw 10 $ wrapComma [text (show x) | x <- [1..10]]
    pw 8  $ wrapInd   [text (show x) | x <- [1..8]]

    --pw 12 $ wrap [text [x] | x <- ['a'..'e']] |+| space <> wrap [text (show x) | x <- [1..10]]
    --pw 10 $ wrap [text [x] | x <- ['a'..'e']] |+| space <> wrap [text (show x) | x <- [1..10]]
    --pw 10 $ wrap [text [x] | x <- ['a'..'f']] |+| space <> wrap [text (show x) | x <- [1..10]]

    putStrLn "parallel columns of wrapped words:"
    pw 23 $ wrap [text [x]      | x <- ['a'..'e']] |+| space <>
            wrap [text (show x) | x <- [  1..10 ]] |+| space <>
            wrap [text [x, x]   | x <- ['a'..'p']]

    putStrLn "elements shared between variants:"
    pp $ runFmt $ do sh1 <- partial $ text "a" <> text "b"
                     sh2 <- partial $ text "1" <> text "2" <> text "3"
                     pure $ text "first" <+> sh1 <+> sh2 <|> sh1 <+> text "second" <+> sh2

    putStrLn "local modification of options:"
    let par content = local (\opt -> opt { costChar = 2 }) $ text "(" <> content <> text ")"
    pp $ text "(" <> text "short" <> text ")" <|> text "longer-text"
    pp $ par (text "short") <|> text "longer-text"

    putStrLn "change cost of element:"
    pp $ text "one" <|> text "two"
    pp $ incCost 0.1 (text "one") <|> text "two"

    --pw 20 $ runFmt $ do sh1 <- partial $ wrap [text (show x) | x <- [1..20]]
    --                    sh2 <- partial $ wrap [text (show x) | x <- [1..10]]
    --                    pure $ (sh1 |+| space <> sh2) $$
    --                           text "" $$
    --                           (sh2 |+| space <> sh1)


