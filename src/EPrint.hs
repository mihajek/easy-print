module EPrint (
    pprintString, pprintBString, pprintText, pprintIO,

    module EPrint.Options,
    module EPrint.Layout,
    module EPrint.Solution,
    module EPrint.Format
) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL

import EPrint.Options
import EPrint.AText
import EPrint.Layout
import EPrint.Solution
import EPrint.Format

ppATs :: Options -> Doc -> [[AText]]
ppATs opt doc = layToATexts $ solLayout (solveDoc opt doc) (lineWidth opt)

pprintString :: Options -> Doc -> String
pprintString opt doc = atToString $ ppATs opt doc

pprintBString :: Options -> Doc -> BL.ByteString
pprintBString opt doc = atToBString $ ppATs opt doc

pprintText :: Options -> Doc -> TL.Text
pprintText opt doc = atToText $ ppATs opt doc

pprintIO :: Options -> Doc -> IO ()
pprintIO opt doc = atPrint $ ppATs opt doc

