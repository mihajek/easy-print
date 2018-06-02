{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module EPrint.AText (
    -- abstract text
    AText(..), TextLike(..),

    -- conversions of abstract text to some real text formats
    atToString, atToBString, atToText, atPrint
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TS
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.List


-- This is abstract text - single line thing which has length, can be printed or converted to String, ByteString and Text
-- (yet another way to deal with Haskell string schizophrenia)
data AText = forall a. TextLike a => AText a

class TextLike a where
    tlLength :: a -> Int
    tlPrint  :: a -> IO ()
    tl2str   :: a -> String
    tl2bs    :: a -> BS.ByteString
    tl2text  :: a -> TS.Text

instance TextLike String where
    tlLength = length
    tlPrint  = putStr
    tl2str   = id
    tl2bs    = encodeUtf8 . TS.pack
    tl2text  = TS.pack

-- note that ByteString variant is required to be UTF8
instance TextLike BS.ByteString where
    tlLength = BS.length
    tlPrint  = BS.putStr
    tl2str   = TS.unpack . decodeUtf8
    tl2bs    = id
    tl2text  = decodeUtf8

instance TextLike TS.Text where
    tlLength = TS.length
    tlPrint  = TS.putStr
    tl2str   = TS.unpack
    tl2bs    = encodeUtf8
    tl2text  = id

instance TextLike AText where
    tlLength (AText t) = tlLength t
    tlPrint  (AText t) = tlPrint t
    tl2str   (AText t) = tl2str t
    tl2bs    (AText t) = tl2bs t
    tl2text  (AText t) = tl2text t

atToString :: [[AText]] -> String
atToString atl = concat $ intersperse "\n" $ map (concat . map tl2str) atl

atToBString :: [[AText]] -> BL.ByteString
atToBString atl = BL.fromChunks $ concat $ intersperse [BS.singleton '\n'] $ map (map tl2bs) atl

atToText :: [[AText]] -> TL.Text
atToText atl = TL.fromChunks $ concat $ intersperse [TS.singleton '\n'] $ map (map tl2text) atl

atPrint :: [[AText]] -> IO ()
atPrint atl = mapM_ printLine atl where
    printLine ln = do mapM_ tlPrint ln
                      putChar '\n'

