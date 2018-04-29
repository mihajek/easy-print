module EPrint (
    pprint, pprintIO,

    module EPrint.Options,
    module EPrint.Layout,
    module EPrint.Solution,
    module EPrint.Format
) where

import EPrint.Options
import EPrint.Layout
import EPrint.Solution
import EPrint.Format

pprint :: Options -> Doc -> String
pprint opt doc = printLayout $ solLayout (solveDoc opt doc) (lineWidth opt)

pprintIO :: Options -> Doc -> IO ()
pprintIO opt doc = printLayoutIO $ solLayout (solveDoc opt doc) (lineWidth opt)

