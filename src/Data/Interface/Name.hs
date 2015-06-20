{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Interface.Name where

import Data.Coerce

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Interface.Source


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
    deriving (Show, Eq, Ord, Functor)

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

instance HasRawName RawName where
    rawName = id
    {-# INLINABLE rawName #-}

instance HasRawName (Name s) where
    -- rawName (Name s) = s
    rawName = coerce
    {-# INLINABLE rawName #-}

instance HasRawName SomeName where
    rawName (SomeName _ s) = s

instance (HasRawName n) => HasRawName (Qual n) where
    rawName (Qual _ n) = rawName n


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

data Named a = Named !RawName !Origin a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

namedThing :: Named a -> a
namedThing (Named _ _ a) = a

origin :: Named a -> Origin
origin (Named _ o _) = o

instance HasRawName (Named a) where
    rawName (Named n _ _) = n

instance HasNamespace a => HasNamespace (Named a) where
    namespace (Named _ _ a) = namespace a

instance HasNamespace a => HasSomeName (Named a) where


-- * QualContext

type QualContext = Set ModuleName

-- | A context where all names are fully qualified
qualifyAll :: QualContext
qualifyAll = Set.empty

-- | The qualified or unqualified name, depending on context.
resolveQual :: (HasRawName n) => QualContext -> Qual n -> String
resolveQual qc qualName@(Qual modName n)
    | modName `Set.member` qc = rawName n
    | otherwise = showQualName qualName


-- * NameMap

type NameMap = Map RawName

nameMapFromList :: (HasRawName a) => [a] -> NameMap a
nameMapFromList = Map.fromList . map (\x -> (rawName x, x))

lookupName :: (HasName s n, HasName s a) => n -> NameMap a -> Maybe a
lookupName = Map.lookup . rawName
{-# INLINABLE lookupName #-}

lookupRawName :: RawName -> NameMap a -> Maybe a
lookupRawName = Map.lookup
{-# INLINABLE lookupRawName #-}
