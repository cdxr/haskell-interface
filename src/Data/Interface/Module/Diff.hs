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





{-
data ExportDiff
    = LocalValueDiff (Named ValueDeclDiff)          -- a ValueDeclDiff
    | LocalTypeDiff (Named TypeDeclDiff)            -- a TypeDeclDiff
    | ExportDiff (Change Export)                    -- none of the above
    deriving (Show, Eq, Ord)


exportNoDiff :: Export -> ExportDiff
exportNoDiff e = case e of
    LocalValue vd -> LocalValueDiff $ mapNamed noDiff vd
    LocalType  td -> LocalTypeDiff  $ mapNamed noDiff td
    ReExport{} -> ExportDiff $ NoChange e


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
            | Just valDiff <- mergeNamedMatch diff vd0 vd1 ->
                LocalValueDiff valDiff
        Change (LocalType td0) (LocalType td1)
            | Just typeDiff <- mergeNamedMatch diff td0 td1 ->
                LocalTypeDiff typeDiff
        _ -> ExportDiff c
-}

diffModuleExports :: ModuleDiff -> [ExportElem]
diffModuleExports mdiff =
    map exportElem . ordSetDiffElems $ diffModuleExportList mdiff
  where
    exportElem :: SetElem ExportName -> ExportElem
    exportElem e = lookupExportElem e mdiff



{-
data ExportDiffSummary = ExportDiffSummary
    { removedExports   :: Set Export
    , addedExports     :: Set Export
    , unchangedExports :: Set Export
    , changedExports   :: Set ExportDiff
    } deriving (Show, Eq, Ord)

instance Monoid ExportDiffSummary where
    mempty = ExportDiffSummary Set.empty Set.empty Set.empty Set.empty
    mappend a b = ExportDiffSummary
        { removedExports   = on Set.union removedExports a b
        , addedExports     = on Set.union addedExports a b
        , unchangedExports = on Set.union unchangedExports a b
        , changedExports   = on Set.union changedExports a b
        }

-- TODO: make ExportDiff an instance of `Diff Export`, simplifying the
--  Elem case below.
singleExportDiff :: Elem ExportDiff Export -> ExportDiffSummary
singleExportDiff el = case el of
    Removed e  -> mempty { removedExports = Set.singleton e }
    Added e    -> mempty { addedExports   = Set.singleton e }
    Elem ediff -> case exportDiffChange ediff of
        NoChange e -> mempty { unchangedExports = Set.singleton e }
        Change{} ->   mempty { changedExports = Set.singleton ediff }

diffExportSummary :: ModuleDiff -> ExportDiffSummary
diffExportSummary = foldMap singleExportDiff . diffModuleExports
-}

