{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-| Types for top-level declarations.
-}
module Data.Interface.Module.Decl
 (
    SomeDecl(..)
  , someDeclName

  , Type
  , ValueDecl(..)
  , typeOf

  , Kind
  , TypeDecl(..)
  , kindOf
 )
where

import Data.Interface.Name
import Data.Interface.Type


data ValueDecl
    = Value Type
    | PatternSyn Type
    | DataCon Type
    deriving (Show, Eq, Ord)

typeOf :: ValueDecl -> Type
typeOf vd = case vd of
    Value t -> t
    PatternSyn t -> t
    DataCon t -> t

instance HasNamespace ValueDecl where
    type Space ValueDecl = 'Just 'Values
    namespace _ = Values


data TypeDecl
    = DataType Kind
    | TypeSyn Kind String
    | TypeClass Kind
    deriving (Show, Eq, Ord)

kindOf :: TypeDecl -> Kind
kindOf td = case td of
    DataType k -> k
    TypeSyn k _ -> k
    TypeClass k -> k

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


data SomeDecl
    = SomeValue !(Named ValueDecl)
    | SomeType  !(Named TypeDecl)
    deriving (Show, Eq, Ord)

someDeclName :: SomeDecl -> SomeName
someDeclName sd = case sd of
    SomeValue decl -> SomeName Values (rawName decl)
    SomeType decl  -> SomeName Types (rawName decl)


instance HasRawName SomeDecl where
    rawName sd = case sd of
        SomeValue n -> rawName n
        SomeType  n -> rawName n

instance HasNamespace SomeDecl where
    type Space SomeDecl = 'Nothing

    namespace SomeValue{} = Values
    namespace SomeType{}  = Types

instance HasSomeName SomeDecl where
