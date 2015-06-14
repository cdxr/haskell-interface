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

  , Kind
  , TypeDecl(..)
 )
where

import Data.Interface.Name


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
