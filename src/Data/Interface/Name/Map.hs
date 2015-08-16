{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interface.Name.Map
(
 -- * NameMap Type
    NameMap'
  , NameMap
  , SomeNameMap

-- * Construction
  , emptyNameMap
  , makeNameMap
  , nameMapFromList
  , someNameMapFromList

 -- * Query
  , viewNameMap
  , lookupName
  , lookupSomeName
  , lookupRawName

 -- ** Insert
  , insertNamed
  , insertHasName

 -- ** Delete
  , deleteName
  , deleteSomeName
  , deleteRawName

 -- ** Transformation
  , renameMap
  , filterMapNames
  , traverseNamedElements

 -- * NameMapDiff
  , NameMapDiff'
  , NameMapDiff
)
where

import Data.Coerce

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Interface.Name
import Data.Interface.Change


-- | An associative map from names to values.
newtype NameMap' n a = NameMap { viewNameMap :: Map n a }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Monoid)

type NameMap = NameMap' RawName
type SomeNameMap = NameMap' SomeName


onInnerMap ::
    (Map n a -> Map n a) ->
    (NameMap' n a -> NameMap' n a)
onInnerMap f = coerce f
{-# INLINABLE onInnerMap #-}

onInnerMapF ::
    (Functor f) =>
    (Map n a -> f (Map n a)) ->
    (NameMap' n a -> f (NameMap' n a))
onInnerMapF f = fmap coerce . f . coerce
{-# INLINABLE onInnerMapF #-}


makeNameMap :: (Ord n) => [Named' n a] -> NameMap' n a
makeNameMap = NameMap . Map.fromList . map (\(Named n a) -> (n, a))

someNameMapFromList :: (HasSomeName a) => [a] -> SomeNameMap a
someNameMapFromList = NameMap . Map.fromList . map (\x -> (someName x, x))

nameMapFromList :: (HasRawName a) => [a] -> NameMap a
nameMapFromList = NameMap . Map.fromList . map (\x -> (rawName x, x))


emptyNameMap :: NameMap' n a
emptyNameMap = NameMap Map.empty

insertNamed :: (Ord n) => Named' n a -> NameMap' n a -> NameMap' n a
insertNamed (Named n a) = onInnerMap $ Map.insert n a

insertHasName :: (HasRawName a) => a -> NameMap a -> NameMap a
insertHasName a = onInnerMap $ Map.insert (rawName a) a

lookupName ::
    (HasName s n, Space a ~ s) =>   -- avoid mismatching namespaces
    n -> NameMap a -> Maybe (Named a)
lookupName = lookupRawName
{-# INLINABLE lookupName #-}

lookupRawName :: (HasRawName n) => n -> NameMap a -> Maybe (Named a)
lookupRawName n = fmap (Named name) . Map.lookup name . viewNameMap
  where
    name = rawName n

lookupSomeName :: (HasSomeName n) => n -> SomeNameMap a -> Maybe (SomeNamed a)
lookupSomeName n = fmap (Named name) . Map.lookup name . viewNameMap
  where
    name = someName n

deleteName :: (HasName s n, HasName s a) => n -> NameMap a -> NameMap a
deleteName = deleteRawName
{-# INLINABLE deleteName #-}

deleteRawName :: (HasRawName n) => n -> NameMap a -> NameMap a
deleteRawName n = onInnerMap $ Map.delete (rawName n)

deleteSomeName :: (HasSomeName n) => n -> SomeNameMap a -> SomeNameMap a
deleteSomeName n = onInnerMap $ Map.delete (someName n)


renameMap ::
    (Ord n, HasRawName n) =>
    (RawName -> RawName) ->             -- injective renaming function
    NameMap' n a -> NameMap' n a
renameMap ren = onInnerMap $ Map.mapKeys (rename ren)


traverseNamedElements ::
    (Applicative f, Ord n) =>
    (Named' n a -> f (Named' n a)) ->
    NameMap' n a -> f (NameMap' n a)
traverseNamedElements f =
    onInnerMapF $ traverseMapPairs (fmap pair . f . uncurry Named)
  where
    pair :: Named' n a -> (n, a)
    pair (Named n a) = (n, a)


-- | Remove all @(name, value)@ pairs in which @name@ or @value@ contains
-- a name that fails to satisfay the predicate.
filterMapNames ::
    (TraverseNames n, TraverseNames a) =>
    (RawName -> Bool) ->
    NameMap' n a -> NameMap' n a
filterMapNames f =
    onInnerMap $ Map.filterWithKey $ \k a -> allNames f k && allNames f a
    
traverseMapPairs ::
    (Applicative f, Ord k) =>
    ((k,a) -> f (k,a)) ->
    Map k a -> f (Map k a)
traverseMapPairs f =
    fmap Map.fromList . traverse f . Map.toList

-- | includes names in keys and values
instance (Ord n, TraverseNames n, TraverseNames a) =>
        TraverseNames (NameMap' n a) where

    traverseNames f = onInnerMapF $ traverseMapPairs onPair
      where
        onPair (k,a) = (,) <$> traverseNames f k <*> traverseNames f a


type NameMapDiff' n = ElemChanges (NameMap' n)
type NameMapDiff = NameMapDiff' RawName

instance (Ord n, ToChange a c) =>
        ToChange (NameMap' n a) (ElemChanges (NameMap' n) c a) where
    toChange = coerce . toChange . transElemChanges viewNameMap

instance (Ord n, Diff a c) =>
        Diff (NameMap' n a) (ElemChanges (NameMap' n) c a) where

    diff a b = transElemChanges NameMap $ diff (viewNameMap a) (viewNameMap b)
    noDiff = NoElemChanges
