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

import Data.Void ( Void, absurd )

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map ( Map )
import qualified Data.Map as Map


-- | @Change a@ represents an "old" value of @a@ paired with a corresponding
-- "new" value of @a@. There is a distinguished constructor for when these
-- values are considered equal, which depends on the context in which the
-- `Change` is used.
data Change a = Same a | Change a a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Change where
    pure = Same

    Same f <*> c = f <$> c
    Change f0 f1 <*> Same a = Change (f0 a) (f1 a)
    Change f0 f1 <*> Change a0 a1 = Change (f0 a0) (f1 a1)


-- | @Replace a@ is like @Change a@, except that the values are always
-- considered different. It should appear in contexts where the values have
-- been statically determined to be different.
data Replace a = Replace a a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


-- | Class for any type @c@ that represents a pair of values of type @a@,
-- where one value is considered an updated or more recent version of the
-- other.
--
-- @
-- toChange (diff a b) = Change c d  ==>  (a,b) = (c,d)
-- @
--
class Diff a c | c -> a where
    diff :: a -> a -> c
    toChange :: c -> Change a

    isChanged :: c -> Bool
    isChanged c = case toChange c of
        Same{}   -> False
        Change{} -> True

    {-# MINIMAL diff, toChange #-}


-- | @old c@ is the older value of @a@ stored in the @c@.
--
-- @
-- old (diff a b) = a
-- @
old :: (Diff a c) => c -> a
old c = case toChange c of
    Change a _ -> a
    Same a     -> a

-- | @new c@ is the newer value of @a@ stored in the @c@.
--
-- @
-- new (diff a b) = b
-- @
new :: (Diff a c) => c -> a
new c = case toChange c of
    Change _ b -> b
    Same b     -> b

-- | @same c@ is @Just a@ when @a = old c = new c@.
same :: (Diff a c) => c -> Maybe a
same c = case toChange c of
    Same a -> Just a
    Change{} -> Nothing

toReplace :: (Diff a c) => c -> Replace a
toReplace c = case toChange c of
    Same a -> Replace a a
    Change a b -> Replace a b


instance (Eq a) => Diff a (Change a) where
    diff a b
        | a /= b    = Change a b
        | otherwise = Same b

    toChange = id
    {-# INLINABLE toChange #-}


-- | trivial: considers all values to be distinct
instance Diff a (Replace a) where
    diff = Replace
    toChange (Replace a b) = Change a b


-- | When @c@ represents a change to a value of type @c@, @Elem c a@ is 
-- a change, a newly-added @a@, or a removed @a@.
data Elem c a
    = Removed a   -- ^ the old value
    | Added a     -- ^ the new value
    | Changed c   -- ^ the old and new value
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

isElemChanged :: (Diff a c) => Elem c a -> Bool
isElemChanged e = case e of
    Removed{} -> True
    Added{}   -> True
    Changed c -> isChanged c


-- | An `Elem` that can never represent a change, only an addition or removal.
type Elem' = Elem Void

-- | Recover the @a@ when you don't care whether it was added or removed.
extractElem :: Elem' a -> a
extractElem e = case e of
    Removed a -> a
    Added a   -> a
    Changed c -> absurd c


-- | An @Elem (Change a) a@.
-- This is named for the class instance @Diff a (Change a)@, which uses the
-- notion of equality provided by `Eq`.
type ElemEq a = Elem (Change a) a

instance Bifunctor Elem where
    bimap f g e = case e of
        Removed a -> Removed (g a)
        Added a   -> Added (g a)
        Changed c -> Changed (f c)


diffMaybe :: (Diff a c) => Maybe a -> Maybe a -> Maybe (Elem c a)
diffMaybe Nothing Nothing = Nothing
diffMaybe (Just a) Nothing = Just $ Removed a
diffMaybe Nothing (Just b) = Just $ Added b
diffMaybe (Just a) (Just b) = Just $ Changed (diff a b)


-- * Computing Diffs for structures

-- ** Set

type DiffSet c a = [Elem c a]
type DiffSetEq a = [ElemEq a]

-- | The differences in element inclusion between two sets
diffSet :: (Ord a) => Set a -> Set a -> DiffSetEq a
diffSet xs0 ys0 = go (Set.toAscList xs0) (Set.toAscList ys0)
  where
    go [] ys = map Added ys
    go xs [] = map Removed xs
    go (x:xs) (y:ys) = case compare x y of
        EQ -> Changed (Same y) : go xs ys
        LT -> Removed x : go xs (y:ys)
        GT -> Added y : go (x:xs) ys

diffSetSummary :: (Diff a c) => DiffSet c a -> DiffSummary c a
diffSetSummary = transDiffSummary reverse . foldr (addDiffElem (:)) mempty


-- ** Map

type DiffMap k c a = Map k (Elem c a)
type DiffMapEq k a = Map k (ElemEq a)

diffMap :: (Ord k, Diff a c) => Map k a -> Map k a -> DiffMap k c a
diffMap = diffMapWith (const diff)

diffMapWith :: (Ord k) =>
    (k -> a -> a -> c) ->
    Map k a -> Map k a -> DiffMap k c a
diffMapWith mkDiff = Map.mergeWithKey combine only1 only2
  where
    combine k x y = Just $ Changed (mkDiff k x y)
    only1 = Map.map Removed
    only2 = Map.map Added


diffMapSummary' ::
    (Diff a c, Ord k) =>
    DiffMap k c a -> DiffSummary' (Map k) c a
diffMapSummary' = foldr f mempty . Map.toList
  where
    f :: (Diff a c, Ord k) =>
         (k, Elem c a) ->
         DiffSummary' (Map k) c a ->
         DiffSummary' (Map k) c a
    f (k, d) = addDiffElem (Map.insert k) d

diffMapSummary :: (Diff a c, Ord k) => DiffMap k c a -> DiffSummary c a
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
    (Diff a c) =>
    (forall b. b -> f b -> f b) ->
    c -> DiffSummary' f c a -> DiffSummary' f c a
addDiff add c ds = case same c of
    Just a  -> ds { unchanged = add a (unchanged ds) }
    Nothing -> ds { changed   = add c (changed ds) }

addDiffElem ::
    (Diff a c) =>
    (forall b. b -> f b -> f b) ->
    Elem c a -> DiffSummary' f c a -> DiffSummary' f c a
addDiffElem add e ds = case e of
    Removed a -> ds { removed   = add a (removed ds) }
    Added a   -> ds { added     = add a (added ds)}
    Changed c -> addDiff add c ds
