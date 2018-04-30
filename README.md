# EPrint
This is prototype implementation of formatter library based on technique described in "A New Approach to Optimal Code Formatting"
by Phillip M. Yelland (though the implementation differs from the paper). Given formatting tree, which describes layout variants
of elements together with their costs, it computes the optimal layout of the whole.

Note that used approach to pretty printing is not particularly fast (see [Performance remarks](README.md#performance-remarks)),
but rather tries to offer features not available elsewhere.

Also note that this library is not well tested, not much optimized and sparsely documented (but sources are self explanatory).
It is just proof of concept prototype, written with purpose to test boundaries of what such simple design can do
(in core, its just three simple functions which combine lists of positions on line).

## Showcase
Here are some examples of what the EPrint can do, but your ordinary pretty printer cannot:

Select layout depending on cost of variants, where selection may dictate possible variants of sub-elements. You define
possible combinations and costs of relevant elements and let the EPrint find the optimal layout in given context.
For simple example, selection between following two variants may depend on cost of prefix vs infix call form (prefix form
needs parentheses around nested calls) and on cost penalty of portion enclosed in parentheses.
```
fun (f a) (f b)
f a `fun` f b
```

Table of values aligned into columns.
```
up     yellow one
middle green  two
down   red    three
```

Table where columns are justified to left, center and right respectively.
```
one-two-three  one-two-three  one-two-three
one-two           one-two           one-two
one                 one                 one
```

Columns of wrapped words, where the width of individual columns is optimized, so that all columns have same height.
This is formatted for line width of 25.
```
a b   1 2 3   aa bb cc dd
c d   4 5 6   ee ff gg hh
e f   7 8 9   ii jj kk ll
g h   10 11   mm nn oo pp
```

## Usage
Helper functions called in examples:
```haskell
pw w doc = pprintIO defaultFmt { lineWidth = w } doc
pp = pw 80
```
Resulting text layout of examples starts with vertical bar and is also terminated
with vertical bar if line width limit is significant in that layout.

Following are examples of provided features:

Tail concatenation
```haskell
pp $ text "a" <> text "b"
pp $ text "a" <+> text "b"
```
```
|ab
|a b
```

Vertical concatenation
```haskell
pp $ text "a" $$ text "b"
```
```
|a
|b
```

Combination of vertical and horizontal concatenation
```haskell
pp $ text "a" <+> text "1" $$
     text "b" <+> text "2"
```
```
|a 1
|b 2
```

Tail goes to last row of vertical layout
```haskell
pp $ (text "a" $$
      text "b") <+> text "tail"
```
```
|a
|b tail
```

Vertical layout as tail of horizontal one
```haskell
pp $ text "a" <+> (text "1" $$
                   text "2")
```
```
|a 1
|  2
```

Choose cheaper variant
```haskell
pw 10 $ text "longer-than-10" <|> text "short"
```
```
|short     |
```

Prefer first if same expensive
```haskell
pp $ text "one" <|> text "two"
```
```
|one
```

Choose between horizontal and vertical layout
```haskell
pp   $ text "one" </> text "two"
```
```
|one two
```
```haskell
pw 5 $ text "one" </> text "two"
```
```
|one  |
|two  |

```

Parallel columns
```haskell
pp $ (text "a" $$ text "b" $$ text "c") |+| (text "1" $$ text "2")
```
```
|a 1
|b 2
|c
```
```haskell
pp $ (text "a" $$ text "b") |+| (text "1" $$ text "2" $$ text "3")
```
```
|a 1
|b 2
|  3
```

Columns justification
```haskell
pp $ toLeft   (text "one-two-three" $$ text "one-two" $$ text "one") |+|
     toCenter (text "one-two-three" $$ text "one-two" $$ text "one") |+|
     toRight  (text "one-two-three" $$ text "one-two" $$ text "one")
```
```
|one-two-three one-two-three one-two-three
|one-two          one-two          one-two
|one                one                one
```

Single row justification
```haskell
pp $ text "one-two" $$ toCenter (text "one") $$ toRight (text "two")
```
```
|one-two
|  one
|    two
```

Wrap words to width
```haskell
pw 12 $ wrap      [text (show x) | x <- [1..20]]
```
```
|1 2 3 4 5 6 |
|7 8 9 10 11 |
|12 13 14 15 |
|16 17 18 19 |
|20          |
```
```haskell
pw 10 $ wrapComma [text (show x) | x <- [1..10]]
```
```
|1, 2, 3,  |
|4, 5, 6,  |
|7, 8, 9,  |
|10        |
```
```haskell
pw 8  $ wrapInd   [text (show x) | x <- [1..8]]
```
```
|1 2 3 4 |
|    5 6 |
|    7 8 |
```

Parallel columns of wrapped words
```haskell
pw 23 $ wrap [text [x]      | x <- ['a'..'e']] |+| space <>
        wrap [text (show x) | x <- [  1..10 ]] |+| space <>
        wrap [text [x, x]   | x <- ['a'..'p']]
```
```
|a b  1 2 3  aa bb cc dd|
|c d  4 5 6  ee ff gg hh|
|e    7 8 9  ii jj kk ll|
|     10     mm nn oo pp|
```

Elements shared between variants
```haskell
pp $ runFmt $ do sh1 <- partial $ text "a" <> text "b"
                 sh2 <- partial $ text "1" <> text "2" <> text "3"
                 pure $ text "first" <+> sh1 <+> sh2 <|> sh1 <+> text "second" <+> sh2
```
```
|first ab 123
```

Local modification of options - second example prefers longer variant because it is not in parentheses
```haskell
let par content = local (\opt -> opt { costChar = 2 }) $ text "(" <> content <> text ")"
pp $ text "(" <> text "short" <> text ")" <|> text "longer-text"
```
```
|(short)
```
```haskell
pp $ par (text "short") <|> text "longer-text"
```
```
|longer-text
```

Change cost of element - second example prefers second - cheaper - variant
```haskell
pp $ text "one" <|> text "two"
```
```
|one
```
```haskell
pp $ incCost 0.1 (text "one") <|> text "two"
```
```
|two
```

## Future development
Here is a list of planned features:
- make EPrint generic over text types: String, Text, ByteString (also combination of Text for some documents
  which need unicode and ByteString for ascii ones make sense in some scenarios)
- document interface functions using hadoc
- look at interface of other pretty-printers and add some convenience functions into EPrint.hs
- create tests for all layout combinations
- implement testing parser & printer for Haskell and possibly other languages


## Performance remarks
This library offers different approach to text layout than traditional ones, based on Hughes or Wadler designs. It allows
to specify choices between several variants of layouts with different costs and let the library compute the most optimal
(cheapest) combination of layout variants into resulting layout.
Performance wise, this approach is much slower. In case of wrap of large number of words, it can be orders of magnitude
slower than HughesPJ. On the other side, it computes something different too. While HughesPJ computes layout for single
particular line width, this library computes optimal layout for every width up to some line width limit. Why this wasting
of computing power? It is useful when you share such element between several variants of choices which would allow
different width of shared element. In such case you compute solution of shared element once and then reuse several times.
Then imagine that your formatting tree consists of choices between variants with many shared elements, these shared elements
are also choices with shared elements and so on... In case of such complex layouts, the slow approach of this library may
be faster, since its complexity is somewhat bounded (see "A New Approach to Optimal Code Formatting" by Phillip M. Yelland
and also implementation of partial solutions in EPrint.Format).

