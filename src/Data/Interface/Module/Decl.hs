{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-| Types for top-level declarations.
-}
module Data.Interface.Module.Decl
 (
    ValueDecl(..)
  , ValueDeclInfo(..)
  , DataField

  , Kind
  , TypeDecl(..)
  , TypeDeclInfo(..)
  , DataConList(..)
 )
where

import Data.Interface.Name
import Data.Interface.Type


-- TODO: add `Origin` field to ValueDecl and TypeDecl


data ValueDecl = ValueDecl
    { vdType   :: Type
    , vdInfo   :: ValueDeclInfo
    } deriving (Show, Eq, Ord)

data ValueDeclInfo
    = Identifier
    | PatternSyn
    | DataCon [DataField]
    deriving (Show, Eq, Ord)

type DataField = Named ()


type instance Space ValueDecl = 'Values

instance HasNamespace ValueDecl where
    namespace _ = Values

instance TraverseNames ValueDecl where
    traverseNames f (ValueDecl t i) = 
        ValueDecl <$> traverseNames f t <*> traverseNames f i

instance TraverseNames ValueDeclInfo where
    traverseNames f vdi = case vdi of
        DataCon fields -> DataCon <$> traverse (traverseNames f) fields
        _ -> pure vdi


data TypeDecl = TypeDecl
    { tdKind   :: Kind
    , tdInfo   :: TypeDeclInfo
    } deriving (Show, Eq, Ord)


data TypeDeclInfo
    = DataType DataConList      -- ^ data/newtype
    | TypeSyn String            -- ^ type synonym (TODO)
    | TypeClass                 -- ^ type class (TODO)
    deriving (Show, Eq, Ord)

{- TypeDecl notes:
      - TypeSyn contains its definition
            (this is only a String for now, but will have to include
             first-class type information)
      TODO:
        - type/data families
-}

type instance Space TypeDecl = 'Types

instance HasNamespace TypeDecl where
    namespace _ = Types

instance TraverseNames TypeDecl where
    traverseNames f (TypeDecl k i) =
        TypeDecl <$> traverseNames f k <*> traverseNames f i

instance TraverseNames TypeDeclInfo where
    traverseNames f tdi = case tdi of
        DataType dcons -> DataType <$> traverseNames f dcons
        _ -> pure tdi


-- | Data constructors for an algebraic type, or `Abstract` when the data
-- constructors are hidden.
data DataConList = Abstract | DataConList [RawName]
    deriving (Show, Eq, Ord)

instance TraverseNames DataConList where
    traverseNames f dcons = case dcons of
        Abstract -> pure Abstract
        DataConList ns -> DataConList <$> traverse (traverseNames f) ns
