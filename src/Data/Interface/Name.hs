{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Interface.Name where


-- | An unqualified name that could belong to any namespace
type RawName = String
-- TODO:  type RawName = Text

data Namespace = Values | Types
    deriving (Show, Read, Eq, Ord)

-- | A name in either the value-namespace or type-namespace
data Name (s :: Namespace) where
    ValueName :: RawName -> Name 'Values
    TypeName  :: RawName -> Name 'Types

type ValueName = Name 'Values
type TypeName = Name 'Types

deriving instance Show (Name s)
deriving instance Eq (Name s)
deriving instance Ord (Name s)


-- | A name with dynamic namespace
data SomeName = SomeName !Namespace !RawName
    deriving (Show, Read, Eq, Ord)


type ModuleName = String
-- TODO:  type ModuleName = Text

-- | @Qual n@ is a value of type @n@ tagged with a `ModuleName`.
data Qual name = Qual !ModuleName !name
    deriving (Show, Eq, Ord, Functor)

-- | Format a qualified name for display to the user
formatQualName :: (HasRawName n) => Qual n -> String
formatQualName (Qual modName name) = modName ++ '.' : rawName name


class HasNamespace n where
    type Space n :: Maybe Namespace

    namespace :: n -> Namespace

instance HasNamespace (Name s) where
    type Space (Name s) = 'Just s

    namespace ValueName{} = Values
    namespace TypeName{} = Types

instance HasNamespace SomeName where
    type Space SomeName = 'Nothing

    namespace (SomeName s _) = s

instance (HasNamespace n) => HasNamespace (Qual n) where
    type Space (Qual n) = Space n

    namespace (Qual _ name) = namespace name


class HasRawName n where
    rawName :: n -> RawName

instance HasRawName (Name s) where
    rawName (ValueName s) = s
    rawName (TypeName s) = s

instance HasRawName SomeName where
    rawName (SomeName _ s) = s

instance (HasRawName n) => HasRawName (Qual n) where
    rawName (Qual _ name) = rawName name


class (HasNamespace n, HasRawName n) => HasSomeName n where
    someName :: n -> SomeName
    someName n = SomeName (namespace n) (rawName n)

instance HasSomeName (Name s) where
    someName (ValueName s) = SomeName Values s
    someName (TypeName s)  = SomeName Types s

instance HasSomeName SomeName where
    someName = id

instance (HasSomeName n) => HasSomeName (Qual n) where
    someName (Qual _ n) = someName n
