{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Interface.Name where

import Data.Interface.Source


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
-- TODO:  type ModuleName = Text

-- | @Qual n@ is a value of type @n@ tagged with a `ModuleName`.
data Qual n = Qual !ModuleName !n
    deriving (Show, Eq, Ord, Functor)

-- | Format a qualified name for display to the user
formatQualName :: (HasRawName n) => Qual n -> String
formatQualName (Qual modName n) = modName ++ '.' : rawName n


class HasNamespace n where
    -- | @Just@ the statically-known namespace, or @Nothing@ if the namespace
    -- is not determined by the type.
    type Space n :: Maybe Namespace

    -- | The namespace associated with a particular value.
    namespace :: n -> Namespace

instance HasNamespace (Name 'Values) where
    type Space (Name 'Values) = 'Just 'Values

    namespace _ = Values

instance HasNamespace (Name 'Types) where
    type Space (Name 'Types) = 'Just 'Types

    namespace _ = Types

instance HasNamespace SomeName where
    type Space SomeName = 'Nothing

    namespace (SomeName s _) = s

instance (HasNamespace n) => HasNamespace (Qual n) where
    type Space (Qual n) = Space n

    namespace (Qual _ n) = namespace n


-- | Class of types that represent or contain names.
class HasRawName n where
    rawName :: n -> RawName

instance HasRawName (Name s) where
    rawName (Name s) = s

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

instance (HasSomeName n) => HasSomeName (Qual n) where
    someName (Qual _ n) = someName n



data Named s a = Named
    { name       :: !(Name s)
    , origin     :: !Origin
    , namedThing :: a
    } deriving (Show, Eq, Ord, Functor)

instance HasRawName (Named s a) where
    rawName (Named n _ _) = rawName n

instance HasNamespace (Named 'Values a) where
    type Space (Named 'Values a) = 'Just 'Values
    namespace _ = Values

instance HasNamespace (Named 'Types a) where
    type Space (Named 'Types a) = 'Just 'Types
    namespace _ = Types
