{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Data.Interface.Change where

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map ( Map )
import qualified Data.Map as Map


-- * Change

-- | A change in the content or presence of a value.
data Change a
    = Removed a     -- ^ the old value was removed
    | Added a       -- ^ a new value was added
    | Change a a    -- ^ the value has changed
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | The value before the change
old :: Change a -> Maybe a
old c = case c of
    Removed a  -> Just a
    Added _    -> Nothing
    Change a _ -> Just a

-- | The value after the change
new :: Change a -> Maybe a
new c = case c of
    Removed _  -> Nothing
    Added a    -> Just a
    Change _ a -> Just a


-- * Diff

-- | A potential change in the content or presence of a value
data Diff a
    = Same a            -- ^ value has not changed
    | Diff (Change a)   -- ^ value has changed
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

maybeChange :: Diff a -> Maybe (Change a)
maybeChange d = case d of
    Same _ -> Nothing
    Diff c -> Just c

diffSpan :: (Eq a) => a -> a -> Diff a
diffSpan a b
    | a /= b    = Diff (Change a b)
    | otherwise = Same b

diffEq :: (Eq a) => Maybe a -> Maybe a -> Maybe (Diff a)
diffEq = diffBy (==)
{-# INLINABLE diffEq #-}

diffBy :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe (Diff a)
diffBy _ Nothing  Nothing  = Nothing
diffBy _ (Just a) Nothing  = Just $ Diff (Removed a)
diffBy _ Nothing  (Just b) = Just $ Diff (Added b)
diffBy eq (Just a) (Just b)
    | a `eq` b  = Just $ Same b
    | otherwise = Just $ Diff (Change a b)


-- * Computing Diffs for structures

-- ** Set

-- | The differences in element inclusion between two sets
diffSet :: (Ord a) => Set a -> Set a -> [Diff a]
diffSet xs0 ys0 = go (Set.toAscList xs0) (Set.toAscList ys0)
  where
    go [] ys = map (Diff . Added) ys
    go xs [] = map (Diff . Removed) xs
    go (x:xs) (y:ys) = case compare x y of
        EQ -> Same y : go xs ys
        LT -> Diff (Removed x) : go xs (y:ys)
        GT -> Diff (Added y) : go (x:xs) ys


-- ** Map

diffMapEq :: (Ord k, Eq a) => Map k a -> Map k a -> Map k (Diff a)
diffMapEq = diffMapBy (==)
{-# INLINABLE diffMapBy #-}


diffMapBy
    :: (Ord k) => (a -> a -> Bool) -> Map k a -> Map k a -> Map k (Diff a)
diffMapBy eq = Map.mergeWithKey combine only1 only2
  where
    combine _ x y = diffBy eq (Just x) (Just y)

    only1 = Map.map $ Diff . Removed
    only2 = Map.map $ Diff . Added


-- * Type-constrained Changes and Diffs

-- | @ADiff tag@ is a @Diff a@ where @a@ is constrained by a GADT-like @tag a@.
data ADiff tag where
    ADiff :: !(tag a) -> Diff a -> ADiff tag

-- | @AChange tag@ is a @Change a@ where @a@ is constrained by a GADT-like
-- @tag a@.
data AChange tag where
    AChange :: !(tag a) -> Change a -> AChange tag

maybeAChange :: ADiff t -> Maybe (AChange t)
maybeAChange (ADiff t d) = AChange t <$> maybeChange d
