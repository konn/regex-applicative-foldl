# regex-applicative-foldl - Using Applicattive Regex matcher for arbitrary foldables.

## Simple usage
```haskell
ghci> import qualified Control.Foldl as L
ghci> import Text.Regex.Applicative.Foldl
ghci> L.fold (exactMatch $ many $ sym 'c') "ccc"
True
ghci> L.fold (exactMatch $ many $ sym 'c') "ccca"
False
ghci> L.fold (matchAll Greedy $ many $ sym 'c') "abccc"
["", "", "ccc"]
ghci> L.fold (matchAll NonGreedy $ many $ sym 'c') "abccc"
["", "", "", "c", "", "c", "", "c", ""]
```

## Matching against non-list containers

```haskell
ghci> import qualified Data.Text as T
ghci> import Data.Text.Lens         -- from @lens@ package.
ghci> decimal = read <$> some (psym $ \c -> '0' <= c && c <= '9') :: RE Char Int
ghci> input = T.pack "123abc4abc9999abc0000000" :: T.Text
ghci> L.foldOver text (maximumMatch Greedy decimal) input
Just 9999
ghci> L.foldOver text (minimumMatch Greedy decimal) input
Just 0
ghci> L.foldOver text (longestInfix decimal) input
Just 0
```
## Short-circuiting

```haskell
ghci> L.fold (exactMatch $ many $ sym 'c') ('a' : replicate 10000000000 'c')
-- Won't terminate... 

-- But the following will terminate soon!
ghci> L.foldM (exactMatchE $ many $ sym 'c') ('a' : replicate 10000000000 'c')
Left Nothing
```
