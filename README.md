# regex-applicative-foldl - Using Applicattive Regex matcher for arbitrary foldables.

## Intro

The [`regex-applicative`][regap] is a great package that provides a neat applicative-style interface for regular expression.
However, it only provides a rich interface only for `String`s.

The [`foldl`][foldl] package is another great package that provides a way to fold over `Foldable`-like structures in a systematic and efficient way.

This package, `regex-applicative-foldl`, brings them together to exploit expressivity of `regex-applicative` to matching against any `Foldable`-like structures with `foldl` interface.

[regap]: https://hackage.haskell.org/package/regex-applicative
[foldl]: https://hackage.haskell.org/package/foldl

## Recommended packages

- [`lens`][lens] - as you know, always useful. 
  Traversals like `vectorTraverse`, `bytes` and/or `text` provides a uniform way
  together with `foldl`'s `foldOver` or `handles` combinators.
- [`mono-traversable`][monotra] - `purely ofoldlUnwrap` does the things for `MonoFoldable`-guys.
- [`profunctors`][profunc] - You can transform input types using `Profunctor` structure.
- [`semigroupoids`][semig] - want to fold over `matchAll`-results?
  You can use `Semigroup` structure of `Fold`s and `folded` lens for that purpose.

Beside these, I'm developping [`foldl-extras`][extra] package to provide extra utility combinators and generic abstraction over folds.
This is not released and its interface can change frequently, but you can try it.

[lens]: https://hackage.haskell.org/package/lens
[monotra]: https://hackage.haskell.org/package/mono-traversable
[semig]: https://hackage.haskell.org/package/semigroupoids
[profunc]: https://hackage.haskell.org/package/profunctors
[extra]: https://github.com/konn/foldl-extras

## Showcase
### Simple usage
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

### Matching against non-list containers

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
### Short-circuiting

```haskell
ghci> L.fold (exactMatch $ many $ sym 'c') ('a' : replicate 10000000000 'c')
-- Won't terminate... 

-- But the following will terminate soon!
ghci> L.foldM (exactMatchE $ many $ sym 'c') ('a' : replicate 10000000000 'c')
Left Nothing
```
