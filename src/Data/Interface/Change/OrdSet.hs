{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Interface.Change.OrdSet
(
    OrdSet
  , makeOrdSet
  , filterOrdSet
  , unsafeTraverseOrdSet
  , ordSetIndexMap
  
  , OrdSetDiff
  , ordSetDiffElems
  , lookupElemIndexDiff
) where

import Data.Foldable
import qualified Data.List

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Maybe ( mapMaybe )

import Data.Interface.Change


-- | An ordered set. This is simply a list with the invariant that no two
-- elements are equal.
newtype OrdSet a = OrdSet [a]
    deriving (Show, Eq, Ord, Foldable)

-- | Construct a proper OrdSet by applying `Data.List.nub` to a @[a]@.
-- /O(n^2)/.
makeOrdSet :: (Eq a) => [a] -> OrdSet a
makeOrdSet = OrdSet . Data.List.nub

filterOrdSet :: (a -> Bool) -> OrdSet a -> OrdSet a
filterOrdSet f (OrdSet xs) = OrdSet $ filter f xs

-- | Traverse over the elements of the `OrdSet`. The traversal must be
-- injective to avoid creating duplicate elements.
unsafeTraverseOrdSet ::
    (Applicative f) =>
    (a -> f b) -> OrdSet a -> f (OrdSet b)
unsafeTraverseOrdSet f (OrdSet xs) = OrdSet <$> traverse f xs


-- | Construct a `Map` from elements to indices.
ordSetIndexMap :: (Ord a) => OrdSet a -> Map a Int
ordSetIndexMap os = foldMap (uncurry Map.singleton) $ zip (toList os) [0..]

instance (Eq a) => Monoid (OrdSet a) where
    mempty = OrdSet []
    mappend (OrdSet a) (OrdSet b) = OrdSet $ Data.List.union a b



data OrdSetDiff a = OrdSetDiff
    { oldOrdSet :: OrdSet a
    , newOrdSet :: OrdSet a
    , ixmapOrdSet :: MapDiffEq a Int
    }

ordSetDiffElems :: (Ord a) => OrdSetDiff a -> [SetElem a]
ordSetDiffElems (OrdSetDiff os0 os1 osm) = case osm of
    NoElemChanges{} -> map (Elem . Same) (toList os1)
    ElemChanges m ->
        mapMaybe (go m) . Data.List.nub $ merge (toList os1) (toList os0)
  where
    go :: (Ord a) => Map a (Elem (Change Int) Int) -> a -> Maybe (SetElem a)
    go m x = case Map.lookup x m of
        Nothing -> Nothing
        Just e -> Just $ case e of
            Removed{} -> Removed x
            Added{}   -> Added x
            Elem{}    -> Elem (Same x)

    merge [] ys = ys
    merge (x:xs) ys = x : merge ys xs


lookupElemIndexDiff :: (Ord a) => a -> OrdSetDiff a -> Maybe (ElemEq Int)
lookupElemIndexDiff a = Map.lookup a . viewMapDiff . ixmapOrdSet

instance (Ord a) => Diff (OrdSet a) (OrdSetDiff a) where
    diff a b = OrdSetDiff a b md
      where
        md = diff (ordSetIndexMap a) (ordSetIndexMap b)

    noDiff a = OrdSetDiff a a (noDiff $ ordSetIndexMap a)

    toChange (OrdSetDiff os0 os1 m) = case m of
        NoElemChanges{} -> NoChange os1
        ElemChanges{}   -> Change os0 os1
