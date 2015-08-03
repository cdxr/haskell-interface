{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interface.ModuleDiff where

import Data.Function ( on )
import Data.Bifunctor ( first )
import Data.Set ( Set )

import Data.Interface.Source ( Origin )
import Data.Interface.Module
import Data.Interface.Name
import Data.Interface.Name.Map
import Data.Interface.Change
import Data.Interface.Change.OrdSet
import Data.Interface.Type
import Data.Interface.Type.Diff


-- | A record of all changes and non-changes to a `ModuleInterface`.
-- This contains enough information to recover the `ModuleInterface` from
-- before or after the changes.
--
data ModuleDiff = ModuleDiff
    { diffModuleName       :: Change ModuleName
    , diffModuleTypeCons   :: NameMapDiff (Change TypeCon) TypeCon
    , diffModuleValueDecls :: NameMapDiff ValueDeclDiff ValueDecl
    , diffModuleTypeDecls  :: NameMapDiff TypeDeclDiff TypeDecl
    , diffModuleExportList :: OrdSetDiff ExportName
    , diffModuleInstances  :: Change (Set ClassInstance)  -- TODO
    , diffModuleOrigins    :: NameMapDiff' SomeName (Change Origin) Origin
    }

instance Diff ModuleInterface ModuleDiff where
    diff a b = ModuleDiff
        { diffModuleName       = on diff moduleName a b
        , diffModuleTypeCons   = on diff moduleTypeCons a b
        , diffModuleValueDecls = on diff moduleValueDecls a b
        , diffModuleTypeDecls  = on diff moduleTypeDecls a b
        , diffModuleExportList = on diff moduleExportList a b
        , diffModuleInstances  = on diff moduleInstances a b
        , diffModuleOrigins    = on diff moduleOrigins a b
        }

    toChange mdiff =
        ModuleInterface
            <$> diffModuleName mdiff
            <*> toChange (diffModuleTypeCons mdiff)
            <*> toChange (diffModuleValueDecls mdiff)
            <*> toChange (diffModuleTypeDecls mdiff)
            <*> toChange (diffModuleExportList mdiff)
            <*> toChange (diffModuleInstances mdiff)
            <*> toChange (diffModuleOrigins mdiff)



data ValueDeclDiff = ValueDeclDiff
    { vdTypeDiff :: TypeDiff
    , vdInfoDiff :: Change ValueDeclInfo
    } deriving (Show, Eq, Ord)

instance Diff ValueDecl ValueDeclDiff where
    diff (ValueDecl ta ia) (ValueDecl tb ib) =
        ValueDeclDiff (diff ta tb) (diff ia ib)

    toChange (ValueDeclDiff t i) = ValueDecl <$> toChange t <*> toChange i

    isChanged (ValueDeclDiff t i) = isChanged i || isChanged t



data TypeDeclDiff = TypeDeclDiff
    { tdKindDiff :: Change Kind
    , tdInfoDiff :: Change TypeDeclInfo
    } deriving (Show, Eq, Ord)

instance Diff TypeDecl TypeDeclDiff where
    diff (TypeDecl ka ia) (TypeDecl kb ib) =
        TypeDeclDiff (diff ka kb) (diff ia ib)

    toChange (TypeDeclDiff t i) = TypeDecl <$> toChange t <*> toChange i


data ExportDiff
    = LocalValueDiff (Named ValueDeclDiff)          -- a ValueDeclDiff
    | LocalTypeDiff (Named TypeDeclDiff)            -- a TypeDeclDiff
    | ExportDiff (Change Export)                    -- none of the above
    deriving (Show, Eq, Ord)


exportDiffChange :: ExportDiff -> Change Export
exportDiffChange ediff = case ediff of
    LocalValueDiff vd -> LocalValue <$> traverse toChange vd
    LocalTypeDiff td  -> LocalType <$> traverse toChange td
    ExportDiff c      -> c


diffModuleExports :: ModuleDiff -> [Elem ExportDiff Export]
diffModuleExports mdiff = map go . ordSetDiffElems $ diffModuleExportList mdiff
  where
    go :: SetElem ExportName -> Elem ExportDiff Export
    go e = first makeExportDiff $ lookupExportElem e mdiff

    makeExportDiff :: Change Export -> ExportDiff
    makeExportDiff c = case c of
        NoChange e ->           -- this is only possible if  @isSame mdiff@
            ExportDiff $ NoChange e
        Change (LocalValue vd0) (LocalValue vd1)
            | Just n <- matchNames vd0 vd1 ->
                LocalValueDiff $ named n $ diff (unName vd0) (unName vd1)
        Change (LocalType td0) (LocalType td1)
            | Just n <- matchNames td0 td1 ->
                LocalTypeDiff $ named n $ diff (unName td0) (unName td1)
        _ -> ExportDiff c
    

lookupExportElem ::
    SetElem ExportName -> ModuleDiff -> Elem (Change Export) Export
lookupExportElem e mdiff =
    fmap unsafeFindExport (toChange mdiff) `applyChange` setElemChange e


isLocalElemChange ::
    Elem (Change (Qual a)) (Qual a) ->
    ModuleDiff ->
    Elem (Change Bool) Bool
isLocalElemChange e mdiff = applyChange (flip isLocal <$> toChange mdiff) e
