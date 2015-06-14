{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Interface.Change where

import Data.Bifunctor
import Data.Function ( on )
import Data.Monoid

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map ( Map )
import qualified Data.Map as Map


-- | Class for any type @c@ that represents a pair of values of type @a@,
-- where one value is considered an updated or more recent version of the
-- other.
class Change a c | c -> a where
    -- | @change a b@ is @Just c@ when there are differences between @a@ and
    -- @b@, or @Nothing@ when @a@ and @b@ are considered equivalent.
    change :: a -> a -> Maybe c

    old :: c -> a
    new :: c -> a


diff :: (Change a c) => a -> a -> Diff c a
diff a b = case change a b of
    Just c  -> Diff c
    Nothing -> Same b


instance (Eq a) => Change a (Replace a) where
    change a b
        | a /= b    = Just $ Replace a b
        | otherwise = Nothing

    old (Replace a _) = a
    new (Replace _ b) = b

instance Change a c => Change a (Diff c a) where
    change a b = Diff <$> change a b

    old (Same a) = a
    old (Diff c) = old c

    new (Same a) = a
    new (Diff c) = new c


-- * Change Types

-- | @Change a@ represents an "old" value of @a@ paired with a corresponding
-- "new" value of @a@. The meaning of the @Change@ is entirely dependent on
-- context.
data Replace a = Replace a a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


-- | When @c@ represents a change to a value of type @c@, @Elem c a@ is 
-- a @c@, a newly-added @a@, or a removed @a@.
data Elem c a
    = Removed a   -- ^ the old value
    | Added a     -- ^ the new value
    | Changed c   -- ^ the old and new value
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type ElemEq a = Elem (Replace a) a

instance Bifunctor Elem where
    bimap f g e = case e of
        Removed a -> Removed (g a)
        Added a   -> Added (g a)
        Changed c -> Changed (f c)


-- * Diff

-- | When @c@ represents a change to a value of type @a@, @Diff c a@ is only
-- a _potential_ change.
data Diff c a = Same a | Diff c
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type DiffEq a = Diff (Replace a) a
type DiffElem c a = Diff (Elem c a) a

instance Bifunctor Diff where
    bimap f g d = case d of
        Same a -> Same (g a)
        Diff c -> Diff (f c)

maybeChange :: Diff c a -> Maybe c
maybeChange d = case d of
    Same _ -> Nothing
    Diff c -> Just c

diffEq :: (Eq a) => a -> a -> Diff (Replace a) a
diffEq a b
    | a /= b    = Diff (Replace a b)
    | otherwise = Same b

diffMaybe :: (Eq a) => Maybe a -> Maybe a -> Maybe (DiffElem (Replace a) a)
diffMaybe Nothing Nothing = Nothing
diffMaybe (Just a) Nothing = Just $ Diff (Removed a)
diffMaybe Nothing (Just b) = Just $ Diff (Added b)
diffMaybe (Just a) (Just b)
    | a /= b    = Just $ Diff (Changed (Replace a b))
    | otherwise = Just $ Same b


-- * Computing Diffs for structures

-- ** Set

type DiffSet c a = [Diff (Elem c a) a]
type DiffSetEq a = DiffSet (Replace a) a

-- | The differences in element inclusion between two sets
diffSet :: (Ord a) => Set a -> Set a -> DiffSetEq a
diffSet xs0 ys0 = go (Set.toAscList xs0) (Set.toAscList ys0)
  where
    go [] ys = map (Diff . Added) ys
    go xs [] = map (Diff . Removed) xs
    go (x:xs) (y:ys) = case compare x y of
        EQ -> Same y : go xs ys
        LT -> Diff (Removed x) : go xs (y:ys)
        GT -> Diff (Added y) : go (x:xs) ys

diffSetSummary :: DiffSet c a -> DiffSummary c a
diffSetSummary = transDiffSummary reverse . foldr (addDiffElem (:)) mempty


-- ** Map

type DiffMap k c a = Map k (DiffElem c a)
type DiffMapEq k a = DiffMap k (Replace a) a

diffMap :: (Ord k, Change a c) => Map k a -> Map k a -> DiffMap k c a
diffMap = diffMapWith (const diff)

diffMapWith :: (Ord k) =>
    (k -> a -> a -> Diff c a) ->
    Map k a -> Map k a -> DiffMap k c a
diffMapWith mkDiff = Map.mergeWithKey combine only1 only2
  where
    combine k x y = Just $ first Changed $ mkDiff k x y
    only1 = Map.map $ Diff . Removed
    only2 = Map.map $ Diff . Added


diffMapSummary' :: (Ord k) => DiffMap k c a -> DiffSummary' (Map k) c a
diffMapSummary' = foldr f mempty . Map.toList
  where
    f :: (Ord k) =>
         (k, Diff (Elem c a) a) ->
         DiffSummary' (Map k) c a ->
         DiffSummary' (Map k) c a
    f (k, d) = addDiffElem (Map.insert k) d

diffMapSummary :: (Ord k) => DiffMap k c a -> DiffSummary c a
diffMapSummary = transDiffSummary Map.elems . diffMapSummary'


-- * DiffSummary

data DiffSummary' f c a = DiffSummary
    { unchanged :: f a
    , added     :: f a
    , removed   :: f a
    , changed   :: f c
    } deriving (Show, Eq, Ord)

type DiffSummary = DiffSummary' []


instance (Monoid (f a), Monoid (f c)) => Monoid (DiffSummary' f c a) where
    mempty = DiffSummary mempty mempty mempty mempty
    mappend a b = DiffSummary
        { unchanged = on (<>) unchanged a b
        , added     = on (<>) added a b
        , removed   = on (<>) removed a b
        , changed   = on (<>) changed a b
        } 

transDiffSummary ::
    (forall b. f b -> g b) ->
    DiffSummary' f c a -> DiffSummary' g c a
transDiffSummary f (DiffSummary a b c d) = DiffSummary (f a) (f b) (f c) (f d)

addDiff ::
    (forall b. b -> f b -> f b) ->
    Diff c a -> DiffSummary' f c a -> DiffSummary' f c a
addDiff add d ds = case d of
    Same a -> ds { unchanged = add a (unchanged ds) }
    Diff c -> ds { changed   = add c (changed ds) }

addDiffElem ::
    (forall b. b -> f b -> f b) ->
    Diff (Elem c a) a -> DiffSummary' f c a -> DiffSummary' f c a
addDiffElem add d ds = case d of
    Same a           -> ds { unchanged = add a (unchanged ds) }
    Diff (Removed a) -> ds { removed   = add a (removed ds) }
    Diff (Added a)   -> ds { added     = add a (added ds)}
    Diff (Changed c) -> ds { changed   = add c (changed ds)}
