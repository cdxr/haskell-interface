{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interface.Module.Diff where

import Data.Function ( on )

import Data.Set ( Set )

import Data.Interface.Source ( Origin )
import Data.Interface.Name
import Data.Interface.Name.Map
import Data.Interface.Change
import Data.Interface.Change.OrdSet
import Data.Interface.Type

import Data.Interface.Module.Interface
import Data.Interface.Module.Entity
import Data.Interface.Module.Export


-- | A record of all changes and non-changes to a `ModuleInterface`.
-- This structure contains enough information to recover the `ModuleInterface`
-- from before or after the changes.
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
    noDiff iface = ModuleDiff
        { diffModuleName       = noDiff $ moduleName iface
        , diffModuleTypeCons   = noDiff $ moduleTypeCons iface
        , diffModuleValueDecls = noDiff $ moduleValueDecls iface
        , diffModuleTypeDecls  = noDiff $ moduleTypeDecls iface
        , diffModuleExportList = noDiff $ moduleExportList iface
        , diffModuleInstances  = noDiff $ moduleInstances iface
        , diffModuleOrigins    = noDiff $ moduleOrigins iface
        }

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


lookupExportElem :: SetElem ExportName -> ModuleDiff -> ExportElem
--    SetElem ExportName -> ModuleDiff -> Elem (Change Export) Export
lookupExportElem e mdiff =
    Named (rawName n) $ fromElemChange $ mapElem unName x
  where
    x = (unsafeFindExport <$> toChange mdiff) `applyChange` setElemChange e
    n = extractSetElem e


isLocalElemChange ::
    Elem (Change (Qual a)) (Qual a) ->
    ModuleDiff ->
    Elem (Change Bool) Bool
isLocalElemChange e mdiff = applyChange (flip isLocal <$> toChange mdiff) e


diffModuleExports :: ModuleDiff -> [ExportElem]
diffModuleExports mdiff =
    map exportElem . ordSetDiffElems $ diffModuleExportList mdiff
  where
    exportElem :: SetElem ExportName -> ExportElem
    exportElem e = lookupExportElem e mdiff
