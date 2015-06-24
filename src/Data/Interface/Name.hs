{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Interface.Name where

import Data.Coerce
import Data.Functor.Identity

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Interface.Source ( Origin )


-- * Name Types

-- | An unqualified name that could belong to any namespace
type RawName = String
-- TODO:  type RawName = Text

data Namespace = Values | Types
    deriving (Show, Read, Eq, Ord)

-- | A name with a known namespace.
newtype Name (s :: Namespace) = Name RawName
    deriving (Show, Eq, Ord)

type ValueName = Name 'Values
type TypeName = Name 'Types


-- | A name with dynamic namespace
data SomeName = SomeName !Namespace !RawName
    deriving (Show, Read, Eq, Ord)


type ModuleName = String
-- TODO:  ModuleName must encode precise origin
--          (package w/ version or filename)


-- * Qual

-- | @Qual n@ is a value of type @n@ tagged with a `ModuleName`.
data Qual n = Qual !ModuleName !n
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

qualModuleName :: Qual n -> ModuleName
qualModuleName (Qual mn _) = mn

unqual :: Qual a -> a
unqual (Qual _ a) = a

-- | Format a qualified name for display to the user
showQualName :: (HasRawName n) => Qual n -> String
showQualName (Qual modName n) = modName ++ '.' : rawName n


-- * Type families and typeclasses

-- | Family of types that have an associated namespace.
type family Space n :: Namespace

type instance Space (Name s) = s
type instance Space (Qual n) = Space n
type instance Space (Named a) = Space a


-- | Class of types that encode a namespace.
class HasNamespace n where
    -- | The namespace associated with a particular value.
    -- If @Space n@ is defined, then @namespace x@ is a constant for all 
    -- @x :: n@ and @Space n@ is the promoted value of @namespace x@.
    namespace :: n -> Namespace


instance HasNamespace (Name 'Values) where
    namespace _ = Values

instance HasNamespace (Name 'Types) where
    namespace _ = Types

instance HasNamespace SomeName where
    namespace (SomeName s _) = s

instance (HasNamespace n) => HasNamespace (Qual n) where
    namespace (Qual _ n) = namespace n


-- | Class of types that represent or contain names.
class HasRawName n where
    rawName :: n -> RawName
    rename :: (RawName -> RawName) -> n -> n

instance HasRawName RawName where
    rawName = id
    {-# INLINABLE rawName #-}

    rename = id
    {-# INLINABLE rename #-}

instance HasRawName (Name s) where
    -- rawName (Name s) = s
    rawName = coerce
    {-# INLINABLE rawName #-}

    rename = coerce
    {-# INLINABLE rename #-}

instance HasRawName SomeName where
    rawName (SomeName _ n) = n
    rename f (SomeName ns n) = SomeName ns (f n)

instance (HasRawName n) => HasRawName (Qual n) where
    rawName (Qual _ n) = rawName n
    rename f (Qual m n) = Qual m (rename f n)


-- | Class of types that represent or contain namespaced-names.
class (HasNamespace n, HasRawName n) => HasSomeName n where
    someName :: n -> SomeName
    someName n = SomeName (namespace n) (rawName n)

instance HasSomeName (Name 'Values) where
    someName (Name s) = SomeName Values s

instance HasSomeName (Name 'Types) where
    someName (Name s) = SomeName Types s

instance HasSomeName SomeName where
    someName = id
    {-# INLINABLE someName #-}

instance (HasSomeName n) => HasSomeName (Qual n) where
    someName (Qual _ n) = someName n


-- | If @n@ has a `RawName` and determines a namespace @s@, it has a @Name s@.
type (HasName s n) = (HasRawName n, Space n ~ s)

getName :: (HasName s n) => n -> Name s
getName = coerce . rawName
{-# INLINABLE getName #-}

getQualName :: (HasName s n) => Qual n -> Qual (Name s)
getQualName = fmap getName
{-# INLINABLE getQualName #-}


-- * Named

data Named a = Named !RawName a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

named :: (HasRawName n) => n -> a -> Named a
named n = Named (rawName n)

unName :: Named a -> a
unName (Named _ a) = a


instance HasRawName (Named a) where
    rawName (Named n _) = n
    rename f (Named n a) = Named (rename f n) a

instance HasNamespace a => HasNamespace (Named a) where
    namespace (Named _ a) = namespace a

instance HasNamespace a => HasSomeName (Named a) where


-- * QualContext

-- | A `QualContext` determines which names may be displayed unqualified.
type QualContext = Set ModuleName
-- TODO: ^ QualContext needs more info than just the Module.
--          We should use Origins to identify built-ins.

-- | A context where all names are fully qualified
qualifyAll :: QualContext
qualifyAll = Set.empty

-- | A default `QualContext` that removes the qualifier from some primitives
-- provided by GHC, such as "Int".
defQualContext :: QualContext
defQualContext = Set.fromList ["GHC.Types", "GHC.Classes", "GHC.Show"]
-- TODO: ^ create a more comprehensive list
--          (maybe base this on Origin rather than module)

-- | The qualified or unqualified name, depending on context.
resolveQual :: (HasRawName n) => QualContext -> Qual n -> String
resolveQual qc qualName@(Qual modName n)
    | modName `Set.member` qc = rawName n
    | otherwise = showQualName qualName


-- * NameMap

type NameMap = Map RawName

nameMapFromList :: (HasRawName a) => [a] -> NameMap a
nameMapFromList = Map.fromList . map (\x -> (rawName x, x))

emptyNameMap :: NameMap a
emptyNameMap = Map.empty

insertNamed :: (HasRawName a) => a -> NameMap a -> NameMap a
insertNamed a = Map.insert (rawName a) a

lookupName :: (HasName s n, HasName s a) => n -> NameMap a -> Maybe a
lookupName = Map.lookup . rawName
{-# INLINABLE lookupName #-}

lookupRawName :: RawName -> NameMap a -> Maybe a
lookupRawName = Map.lookup
{-# INLINABLE lookupRawName #-}

deleteName :: (HasName s n, HasName s a) => n -> NameMap a -> NameMap a
deleteName = Map.delete . rawName

renameMap :: (HasRawName a) =>
    (RawName -> RawName) ->             -- ^ injective renaming function
    NameMap a -> NameMap a
renameMap ren = Map.map (rename ren) . Map.mapKeys ren

traverseNamedElements ::
    (Applicative f) =>
    (Named a -> f (Named a)) ->
    NameMap a -> f (NameMap a)
traverseNamedElements f =
    traverseMapPairs (fmap pair . f . uncurry Named)
  where
    pair :: Named a -> (RawName, a)
    pair (Named n a) = (n, a)
    
traverseMapPairs ::
    (Applicative f, Ord k) =>
    ((k,a) -> f (k,a)) ->
    Map k a -> f (Map k a)
traverseMapPairs f =
    fmap Map.fromList . traverse f . Map.toList



-- * TraverseNames

-- | A class of types that permit a full traversal of all contained
-- `RawName`s.
class TraverseNames s where
    traverseNames :: (Applicative f) => (RawName -> f RawName) -> s -> f s

-- | When @ren@ is a function from names to names, @renameAll ren@ replaces
-- all names @n@ in a structure with @ren n@. For many structures, @ren@
-- must be injective to preserve its shape.
renameAll :: (TraverseNames a) => (RawName -> RawName) -> a -> a
renameAll ren = coerce . traverseNames go
  where
    go :: RawName -> Identity RawName
    go = coerce . ren

instance TraverseNames () where
    traverseNames _ _ = pure ()

instance TraverseNames RawName where
    traverseNames = id
    {-# INLINABLE traverseNames #-}

instance TraverseNames (Name s) where
    traverseNames f = fmap coerce . f . coerce
    {-# INLINABLE traverseNames #-}

instance (TraverseNames a) => TraverseNames (Named a) where
    traverseNames f (Named n a) = Named <$> f n <*> traverseNames f a

instance (TraverseNames k, TraverseNames a, Ord k) =>
            TraverseNames (Map k a) where
    traverseNames f = traverseMapPairs onPair
      where
        onPair (k,a) = (,) <$> traverseNames f k <*> traverseNames f a

instance (TraverseNames a, Ord a) => TraverseNames (Set a) where
    traverseNames f =
        fmap Set.fromList . traverse (traverseNames f) . Set.toList

{- TODO: after RawName is no longer synonym for [Char]
instance (TraverseNames a) => TraverseNames [a] where
    traverseNames f = traverse (traverseNames f)
-}

instance (TraverseNames a) => TraverseNames (Qual a) where
    traverseNames f = traverse (traverseNames f)

instance TraverseNames SomeName where
    traverseNames f (SomeName ns n) = SomeName ns <$> f n

instance TraverseNames Origin where
    traverseNames _ = pure
