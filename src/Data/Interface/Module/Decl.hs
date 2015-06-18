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
  , DataField

  , Kind
  , TypeDecl(..)
  , kindOf
  , DataConList(..)
 )
where

import Data.Interface.Name
import Data.Interface.Type


data ValueDecl
    = Value Type
    | PatternSyn Type
    | DataCon Type [DataField]
    deriving (Show, Eq, Ord)

type DataField = Named ()

typeOf :: ValueDecl -> Type
typeOf vd = case vd of
    Value t -> t
    PatternSyn t -> t
    DataCon t _ -> t

instance HasNamespace ValueDecl where
    type Space ValueDecl = 'Just 'Values
    namespace _ = Values


data TypeDecl
    = DataType Kind DataConList
    | TypeSyn Kind String
    | TypeClass Kind
    deriving (Show, Eq, Ord)

{- TypeDecl notes:
      - TypeSyn contains its definition
            (this is only a String for now, but will have to include
             first-class type information)
      TODO:
        - type/data families
-}

kindOf :: TypeDecl -> Kind
kindOf td = case td of
    DataType k _ -> k
    TypeSyn k _  -> k
    TypeClass k  -> k

instance HasNamespace TypeDecl where
    type Space TypeDecl = 'Just 'Types
    namespace _ = Types


-- | Data constructors for an algebraic type, or `Abstract` when the data
-- constructors are hidden.
data DataConList = Abstract | DataConList [Named ()]
    deriving (Show, Eq, Ord)


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
