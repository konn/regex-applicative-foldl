{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Text.Regex.Applicative.Foldl
  ( exactMatch,
    exactMatchE,
    matchAll,
    maximumMatch,
    minimumMatch,
    longestInfix,
    shortestInfix,
    module Text.Regex.Applicative,
  )
where

import Control.Arrow (Arrow (first))
import Control.Foldl (Fold (..), FoldM (..))
import qualified Control.Foldl as L
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Semigroupoid (Semigroupoid (o))
import Data.Tuple (swap)
import Text.Regex.Applicative
import Text.Regex.Applicative.Object
  ( compile,
    failed,
    results,
    step,
  )

{- | Returns 'Just' if the /whole/ input matches the given regex.

   __N.B.__ Even if the matching fails in the middle of input sequence,
   this @'Fold'@ won't terminate until all the element is consumed.
   You could use @'exactMatchE'@, which uses 'Either's for early return.
-}
exactMatch :: RE c a -> Fold c (Maybe a)
exactMatch re = Fold (flip step) (compile re) (listToMaybe . results)

{- | Short-circuit version of 'exactMatch'.

 >>> foldM (exactMatchM empty) (replicate maxBound 'c')
 Left Nothing

 >>> either id id $ foldM (exactMatchM empty) (replicate maxBound 'c')
 Nothing
-}
exactMatchE :: RE c a -> FoldM (Either (Maybe a)) c (Maybe a)
exactMatchE re = FoldM go (Right $ compile re) (Right . listToMaybe . results)
  where
    go !obj !c
      | failed obj = Left Nothing
      | otherwise = Right $ step c obj

data P a b = P {getFst :: !a, getSnd :: !b}
  deriving (Read, Show, Eq, Ord)

{- | Finds the /maximum/ matching result w.r.t. the ordering on @a@.

   This exploits the 'Semigroupod' structure of 'Fold's:

 @
 'maximumMatch' re = 'L.handles' 'L.folded' 'L.maximum' \``o`\` 'matchAll' re
 @

  /c.f./ 'minimumMatch', 'longestInfix' and 'matchAll'.
-}
maximumMatch :: Ord a => RE c a -> Fold c (Maybe a)
maximumMatch re =
  L.handles L.folded L.maximum
    `o` matchAll re

{- | Find the /minimum/ matching result w.r.t. the ordering on @a@.

  /c.f./ 'maximumMatch', 'longestInfix' and 'matchAll'.
-}
minimumMatch :: Ord a => RE c a -> Fold c (Maybe a)
minimumMatch re = coerce $ maximumMatch (Down <$> re)

{- | Finds a value of left-most longest infix.

   /c.f./ 'maximumMatch'.
-}
longestInfix ::
  RE c a -> Fold c (Maybe a)
longestInfix re =
  fmap payload
    <$> maximumMatch
      (uncurry Weighted . first length . swap <$> withMatched re)

{- | Finds a value of left-most shortest infix.

   /c.f./ 'longstInfix', 'maximumMatch' and 'minimumMatch'.
-}
shortestInfix ::
  RE c a -> Fold c (Maybe a)
shortestInfix re =
  fmap payload
    <$> minimumMatch
      (uncurry Weighted . first length . swap <$> withMatched re)

data Weighted w a = Weighted {weight :: !w, payload :: !a}
  deriving (Read, Show)

instance Eq w => Eq (Weighted w a) where
  (==) = (==) `on` weight
  (/=) = (/=) `on` weight

instance Ord w => Ord (Weighted w a) where
  compare = comparing weight

data P3 a b c = P3 !a !b !c
  deriving (Read, Show, Eq, Ord)

data Maybe' a = Nothing' | Just' !a
  deriving (Read, Show, Eq, Ord, Functor)

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*> (!_) = Nothing'
  (!_) <*> Nothing' = Nothing'
  Just' f <*> Just' a = Just' (f a)

instance Alternative Maybe' where
  empty = Nothing'
  Nothing' <|> (!m) = m
  (!m) <|> Nothing' = m
  l@Just' {} <|> (!_) = l

{- | Extract all matching subsequence in fold.

__N.B.__ Unlike 'Text.Regex.Applicative.replace',
if the given regex matches empty string,
each empty string position matches /exactly once/.

>>> fold (matchAll (many $ sym 'c')) "abccc"
["", "", "ccc"]
-}
matchAll :: RE c a -> Fold c [a]
{-# INLINE matchAll #-}
matchAll re =
  fromMaybe [] <$> exactMatch (few anySym *> many (re <* few anySym))
