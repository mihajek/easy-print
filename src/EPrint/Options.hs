module EPrint.Options (
    Options(..),
    defaultFmt
) where

-- Formatting options
-- Note that lineWidth in Options is maximal width considered during computation of solution. It can be set to big number to get
-- line width independent solution and then solution can be used to print layouts for different particular line widths.
data Options = Options {
    lineWidth          :: !Int,      -- line width in characters
    costChar           :: !Double,   -- cost per character below width
    costLeftOver       :: !Double,   -- cost per character over width - leftover/overrun/overflow
    costLineBreak      :: !Double,   -- default cost of line break if not locally modified inside block

    -- horizontal concatenation cost variance coefficient can be used to fine-tune solCatHMulti inclusion of more expensive
    -- combinations into consideration for following step (making it less effecient and more precise or vice versa)
    hcatCostVariance   :: !Double    -- reasonable value shall be in range 1..2 where 1 means no variance
} deriving Show

defaultFmt :: Options
defaultFmt = Options {
    lineWidth = 80,
    costChar = 0.1,
    costLeftOver = 10,
    costLineBreak = 1,
    hcatCostVariance = 1.2
}

