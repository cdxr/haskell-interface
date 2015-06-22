{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interface.ModuleDiff where

import Data.Foldable ( toList )
import Data.Function ( on )

import Data.Interface.Module
import Data.Interface.Name
import Data.Interface.Change
import Data.Interface.Type


-- | A record of all changes and non-changes to a `ModuleInterface`.
-- This contains enough information to recover the `ModuleInterface` from
-- before or after the changes.
--
data ModuleDiff = ModuleDiff
    { diffModuleName      :: !(Change ModuleName)
    , diffModuleTypeCons  :: !(DiffMapEq RawName TypeCon)
    , diffModuleExports   :: ![ExportDiff]
    --, diffModuleValueDecls :: !(DiffMap RawName ValueDeclChange (Named ValueDecl))
    --, diffModuleTypeDecls  :: !(DiffMap RawName TypeDeclChange (Named TypeDecl))
    --, diffModuleExportList :: !ExportListDiff  -- TODO
    --, diffModuleInstances  :: !(DiffSetEq ClassInstance)
    } deriving (Show)


diffModules :: ModuleInterface -> ModuleInterface -> ModuleDiff
diffModules a b = ModuleDiff
    { diffModuleName     = on diff moduleName a b
    , diffModuleTypeCons = on diffMap moduleTypeCons a b
    , diffModuleExports  =
        diffExports (moduleName a, compileModuleExports a)
                    (compileModuleExports b)
    --, diffModuleValueDecls = on diffMap moduleValueDecls a b
    --, diffModuleTypeDecls  = on diffMap moduleTypeDecls a b
    --, diffModuleExportList = on diffSet (Set.fromList . moduleExportList) a b
    --, diffModuleInstances  = on diffSet moduleInstances a b
    }


-- TODO make instance Diff Export ExportDiff
--   this requires factoring out the ModuleName parameter from diffExports

data ExportDiff
    = DiffValue (Elem ValueDeclChange (Named ValueDecl))
    | DiffType (Elem TypeDeclChange (Named TypeDecl))
    | SameReExport ExportName
    | DiffReExport (Elem' ExportName)
    deriving (Show, Eq, Ord)

type ValueDeclChange = Change (Named ValueDecl)  -- TODO

{-
data ValueDeclChange =
    ValueDeclChange TypeDiff  -- TODO needs additional data

instance Diff ValueDeclChange ValueDecl where
    diff a b = ValueDeclChange $ diff a b
    toChange (ValueDeclChange tdiff) = toChange
-}


type TypeDeclChange = Change (Named TypeDecl)    -- TODO

diffExports :: (ModuleName, [Export]) -> [Export] -> [ExportDiff]
diffExports (modName, es0) =
    go ( nameMapFromList valueDecls0
       , nameMapFromList typeDecls0
       , nameMapFromList $ reexports es0
       )
  where
    (_names0, valueDecls0, typeDecls0) = splitExports modName es0

    go :: ( NameMap (Named ValueDecl)   -- original values
          , NameMap (Named TypeDecl)    -- original types
          , NameMap ExportName) ->      -- original re-exports
          [Export] ->                   -- new exports
          [ExportDiff]                  -- list of export differences
    go (vds, tds, res) es1 = case es1 of
        [] -> map (DiffValue . Removed) (toList vds)
           ++ map (DiffType . Removed) (toList tds)
           ++ map (DiffReExport . Removed) (toList res)
        e:es -> case e of
            LocalValue vd1
                | Just vd0 <- lookupName vd1 vds ->
                    DiffValue (Changed (diff vd0 vd1))
                        : go (deleteName vd0 vds, tds, res) es
                | otherwise ->
                    DiffValue (Added vd1) : go (vds, tds, res) es
            LocalType td1
                | Just td0 <- lookupName td1 tds ->
                    DiffType (Changed (diff td0 td1))
                        : go (vds, deleteName td0 tds, res) es
                | otherwise ->
                    DiffType (Added td1) : go (vds, tds, res) es
            -- assume re-exports with the same name are equal
            ReExport n1
                | Just n0 <- lookupName n1 res ->
                    SameReExport n1 : go (vds, tds, deleteName n0 res) es
                | otherwise ->
                    DiffReExport (Added n1) : go (vds, tds, res) es
