{-# LANGUAGE BangPatterns #-}
module Text.Regex.Applicative.Foldl
  ( exactMatch, exactMatchE,
    matchAll,
    maximumMatch, minimumMatch,
    longestInfix, shortestInfix,
    matchAllGreedy, matchAllNonGreedy,
    module Text.Regex.Applicative
  ) where
import           Control.Arrow
import           Control.Foldl                 (Fold (..), FoldM (..))
import qualified Control.Foldl                 as L
import           Data.Coerce
import           Data.Foldable
import           Data.Function
import           Data.Maybe
import           Data.Ord
import           Data.Semigroupoid
import qualified Data.Sequence                 as Seq
import           Data.Tuple
import           Text.Regex.Applicative
import           Text.Regex.Applicative.Object

-- | Returns 'Just' if the /whole/ input matches the given regex.
--
--   __N.B.__ Even if the matching fails in the middle of input sequence,
--   this @'Fold'@ won't terminate until all the element is consumed.
--   You could use @'exactMatchE'@, which uses 'Either's for early return.
exactMatch :: RE c a -> Fold c (Maybe a)
exactMatch re = Fold (flip step) (compile re) (listToMaybe . results)

-- | Short-circuit version of 'exactMatch'.
--
-- >>> foldM (exactMatchM empty) (replicate maxBound 'c')
-- Left Nothing
--
-- >>> either id id $ foldM (exactMatchM empty) (replicate maxBound 'c')
-- Nothing
exactMatchE :: RE c a -> FoldM (Either (Maybe a)) c (Maybe a)
exactMatchE re = FoldM go (Right $ compile re) (Right . listToMaybe . results)
  where
    go !obj !c
      | failed obj = Left Nothing
      | otherwise = Right $ step c obj

data P a b = P { getFst :: !a, getSnd :: !b }
  deriving (Read, Show, Eq, Ord)

matchAllNonGreedy :: RE c a -> Fold c [a]
matchAllNonGreedy re = Fold go (P iniMS obj0) (toList . getFst)
  where
    obj0 = compile re
    iniMS = maybe Seq.empty Seq.singleton $ listToMaybe $ results obj0
    go (P ms obj) !c
      | failed obj' = P (ms <> iniMS) obj0
      | not (null res) = P (ms <> Seq.singleton (head res) <> iniMS) obj0
      | otherwise = P ms obj'
      where
        obj' = step c obj
        res = results obj'

-- | Finds the /maximum/ matching result w.r.t. the ordering on @a@.
--
--   This exploits the 'Semigroupod' structure of 'Fold's:
--
-- @
-- 'maximumMatch' g re = 'L.handles' 'L.folded' 'L.maximum' \``o`\` 'matchAll' g re
-- @
--
--  /c.f./ 'minimumMatch', 'longestInfix' and 'matchAll'.
maximumMatch :: Ord a => Greediness -> RE c a -> Fold c (Maybe a)
maximumMatch g re =
  L.handles L.folded L.maximum
    `o`
  matchAll g re

-- | Find the /minimum/ matching result w.r.t. the ordering on @a@.
--
--  /c.f./ 'maximumMatch', 'longestInfix' and 'matchAll'.
minimumMatch :: Ord a => Greediness -> RE c a -> Fold c (Maybe a)
minimumMatch g re = coerce $ maximumMatch g (Down <$> re)

-- | Finds a value of left-most longest infix.
--
--   /c.f./ 'maximumMatch'.
longestInfix
  :: RE c a -> Fold c (Maybe a)
longestInfix re =
  fmap payload <$>
  maximumMatch Greedy
    (uncurry Weighted . first length . swap <$> withMatched re)

-- | Finds a value of left-most shortest infix.
--
--   /c.f./ 'longstInfix', 'maximumMatch' and 'minimumMatch'.
shortestInfix
  :: RE c a -> Fold c (Maybe a)
shortestInfix re =
  fmap payload <$>
  minimumMatch NonGreedy
    (uncurry Weighted . first length . swap <$> withMatched re)

data Weighted w a = Weighted { weight :: !w, payload :: !a }
  deriving (Read, Show)

instance Eq w => Eq (Weighted w a) where
  (==) = (==) `on` weight
  (/=) = (/=) `on` weight

instance Ord w => Ord (Weighted w a) where
  compare = comparing weight

matchAllGreedy :: RE c a -> Fold c [a]
matchAllGreedy re = Fold go (P3 Seq.empty iniRes obj0) ext
  where
    ext (P3 ms prev _) = toList $ ms <> maybeSeq' prev
    obj0 = compile re
    iniRes = result' obj0
    go (P3 ms prev obj) !c
      | failed obj' =
        let obj'' = step c obj0
            ms' = ms <> maybeSeq' prev
            next | failed obj'' = obj0
                 | otherwise    = obj''
        in P3 ms' (result' next) next
      | otherwise = P3 ms (result' obj') obj'
      where
        !obj' = step c obj

maybeSeq' :: Maybe' a -> Seq.Seq a
maybeSeq' (Just' a) = Seq.singleton a
maybeSeq' Nothing'  = Seq.empty

result' :: ReObject s a -> Maybe' a
result' = toMaybe' . listToMaybe . results

data P3 a b c = P3 !a !b !c

data Maybe' a = Nothing' | Just' !a

toMaybe' :: Maybe a -> Maybe' a
toMaybe' (Just a) = Just' a
toMaybe' Nothing  = Nothing'

{- | Extract all matching subsequence in fold.

__N.B.__ Unlike 'Text.Regex.Applicative.replace',
if the given regex matches empty string,
each empty string position matches /exactly once/.

>>> fold (matchAll NonGreedy (optional $ sym 'c')) "abccc"
[Nothing,Nothing,Nothing,Just 'c',Nothing,Just 'c',Nothing,Just 'c',Nothing]

>>> fold (matchAll Greedy (many $ sym 'c')) "abccc"
["", "", "ccc"]
-}
matchAll :: Greediness -> RE c a -> Fold c [a]
{-# INLINE matchAll #-}
matchAll Greedy    = matchAllGreedy
matchAll NonGreedy = matchAllNonGreedy
