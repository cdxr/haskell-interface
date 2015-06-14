{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

{-| Types for top-level declarations.
-}
module Data.Interface.Module.Decl
 (
    NamedDecl(..)
  , NamedValue
  , NamedType
  , declName
  , declOrigin

  , SomeDecl(..)
  , someDecl
  , someDeclName

  , Type
  , ValueDecl(..)

  , Kind
  , TypeDecl(..)
 )
where

import Data.Interface.Name
import Data.Interface.Source


type Type = String      -- ^ TODO

data ValueDecl
    = Value String
    | PatternSyn Type
    | DataCon Type
    deriving (Show, Eq, Ord)

instance HasNamespace ValueDecl where
    type Space ValueDecl = 'Just 'Values
    namespace _ = Values


type Kind = [String]    -- ^ TODO

data TypeDecl
    = DataType Kind
    | TypeSyn Kind String
    | TypeClass Kind
    deriving (Show, Eq, Ord)

instance HasNamespace TypeDecl where
    type Space TypeDecl = 'Just 'Types
    namespace _ = Types

{- TypeDecl notes:
      - TypeSyn contains its definition
            (this is only a String for now, but will have to include
             first-class type information)
      - Type constructors will need a list of their data constructors

      TODO:
        - type/data families
-}


data NamedDecl s where
    NamedValue :: Named 'Values ValueDecl -> NamedDecl 'Values
    NamedType  :: Named 'Types TypeDecl -> NamedDecl 'Types

declName :: NamedDecl s -> Name s
declName (NamedValue n) = name n
declName (NamedType n) = name n

declOrigin :: NamedDecl s -> Origin
declOrigin (NamedValue n) = origin n
declOrigin (NamedType n) = origin n


type NamedValue = NamedDecl 'Values
type NamedType = NamedDecl 'Types


deriving instance Show (NamedDecl s)
deriving instance Eq   (NamedDecl s)
deriving instance Ord  (NamedDecl s)


data SomeDecl
    = SomeValue !(NamedDecl 'Values)
    | SomeType !(NamedDecl 'Types)
    deriving (Show, Eq, Ord)

someDecl :: NamedDecl s -> SomeDecl
someDecl decl = case decl of
    NamedValue{} -> SomeValue decl
    NamedType{}  -> SomeType decl

someDeclName :: SomeDecl -> SomeName
someDeclName sd = case sd of
    SomeValue decl -> SomeName Values (rawName decl)
    SomeType decl  -> SomeName Types (rawName decl)


instance HasNamespace (NamedDecl s) where
    type Space (NamedDecl s) = 'Just s

    namespace (NamedValue n) = namespace n
    namespace (NamedType n) = namespace n

instance HasNamespace SomeDecl where
    type Space SomeDecl = 'Nothing

    namespace SomeValue{} = Values
    namespace SomeType{}  = Types

instance HasRawName (NamedDecl s) where
    rawName decl = case decl of
        NamedValue n -> rawName n
        NamedType n -> rawName n

