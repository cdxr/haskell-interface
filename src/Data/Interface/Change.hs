{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Interface.Change where

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map ( Map )
import qualified Data.Map as Map


-- * Change Types

-- | @Change a@ represents an "old" value of @a@ paired with a corresponding
-- "new" value of @a@. The meaning of the @Change@ is entirely dependent on
-- context.
data Change a = Change a a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


-- | @ElemChange a@ is similar to @Change a@, except that either the old
-- or new value may not be present.
data ElemChange a
    = Removed a     -- ^ the old value
    | Added a       -- ^ the new value
    | Replaced a a  -- ^ the old and new values
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

replace :: Change a -> ElemChange a
replace (Change a b) = Replaced a b


-- * Diff

-- | When @f a@ represents a change to a value of type @a@, @Diff f a@
-- is only a _potential_ change.
data Diff f a = Same a | Diff (f a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

mapDiff :: (a -> b) -> (f a -> g b) -> Diff f a -> Diff g b
mapDiff sameF diffF d = case d of
    Same a  -> Same (sameF a)
    Diff fa -> Diff (diffF fa)

maybeChange :: Diff f a -> Maybe (f a)
maybeChange d = case d of
    Same _ -> Nothing
    Diff c -> Just c

diffEq :: (Eq a) => a -> a -> Diff Change a
diffEq a b
    | a /= b    = Diff (Change a b)
    | otherwise = Same b

diffMaybe :: (Eq a) => Maybe a -> Maybe a -> Maybe (Diff ElemChange a)
diffMaybe Nothing Nothing = Nothing
diffMaybe (Just a) Nothing = Just $ Diff (Removed a)
diffMaybe Nothing (Just b) = Just $ Diff (Added b)
diffMaybe (Just a) (Just b)
    | a /= b    = Just $ Diff (Replaced a b)
    | otherwise = Just $ Same b


-- * Computing Diffs for structures

-- ** Set

-- | The differences in element inclusion between two sets
diffSet :: (Ord a) => Set a -> Set a -> [Diff ElemChange a]
diffSet xs0 ys0 = go (Set.toAscList xs0) (Set.toAscList ys0)
  where
    go [] ys = map (Diff . Added) ys
    go xs [] = map (Diff . Removed) xs
    go (x:xs) (y:ys) = case compare x y of
        EQ -> Same y : go xs ys
        LT -> Diff (Removed x) : go xs (y:ys)
        GT -> Diff (Added y) : go (x:xs) ys


-- ** Map

diffMap :: (Ord k) =>
    (k -> a -> a -> Diff Change a) ->
    Map k a -> Map k a -> Map k (Diff ElemChange a)
diffMap mkDiff = Map.mergeWithKey combine only1 only2
  where
    combine k x y = Just $ mapDiff id replace $ mkDiff k x y
    only1 = Map.map $ Diff . Removed
    only2 = Map.map $ Diff . Added


-- * Type-constrained Changes and Diffs

data TagF f tag where
    TagF :: !(tag a) -> f a -> TagF f tag

type ADiff f = TagF (Diff f)
type AChange = TagF Change
type AnElemChange = TagF ElemChange

maybeAChange :: TagF (Diff f) tag -> Maybe (TagF f tag)
maybeAChange (TagF tag adiff) = TagF tag <$> maybeChange adiff
