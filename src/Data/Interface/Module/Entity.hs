{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Interface.Module.Entity where

import Data.Interface.Change
import Data.Interface.Name
import Data.Interface.Type
import Data.Interface.Type.Diff


-- | A top-level exported entity in a module, without a name.
data Entity
    = LocalValue ValueDecl           -- ^ a value-namespace declaration
    | LocalType TypeDecl             -- ^ a type-namespace declaration
    | ReExport ModuleName Namespace  -- ^ module and namespace of declaration
    deriving (Show, Eq, Ord)


data EntityDiff
    = LocalValueDiff ValueDeclDiff  -- ^ a value-namespace diff
    | LocalTypeDiff TypeDeclDiff    -- ^ a type-namespace diff
    | EntityDiff (Change Entity)    -- ^ none of the above
    deriving (Show, Eq, Ord)


instance Diff Entity EntityDiff where
    noDiff e = case e of
        LocalValue vd -> LocalValueDiff (noDiff vd)
        LocalType td -> LocalTypeDiff (noDiff td)
        ReExport{} -> EntityDiff (NoChange e) 

    diff a b = case (a,b) of
        (LocalValue vd0, LocalValue vd1) -> LocalValueDiff (diff vd0 vd1)
        (LocalType td0, LocalType td1)   -> LocalTypeDiff (diff td0 td1)
        _ -> EntityDiff (Change a b)

    toChange ediff = case ediff of
        LocalValueDiff vd -> LocalValue <$> toChange vd
        LocalTypeDiff td -> LocalType <$> toChange td
        EntityDiff c -> c


-- * ValueDecl

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


data ValueDeclDiff = ValueDeclDiff
    { vdTypeDiff :: TypeDiff
    , vdInfoDiff :: Change ValueDeclInfo
    } deriving (Show, Eq, Ord)

instance Diff ValueDecl ValueDeclDiff where
    diff (ValueDecl ta ia) (ValueDecl tb ib) =
        ValueDeclDiff (diff ta tb) (diff ia ib)

    toChange (ValueDeclDiff t i) = ValueDecl <$> toChange t <*> toChange i



-- * TypeDecl

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


data TypeDeclDiff = TypeDeclDiff
    { tdKindDiff :: Change Kind
    , tdInfoDiff :: Change TypeDeclInfo
    } deriving (Show, Eq, Ord)

instance Diff TypeDecl TypeDeclDiff where
    diff (TypeDecl ka ia) (TypeDecl kb ib) =
        TypeDeclDiff (diff ka kb) (diff ia ib)

    toChange (TypeDeclDiff t i) = TypeDecl <$> toChange t <*> toChange i

