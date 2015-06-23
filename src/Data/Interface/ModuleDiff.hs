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
import Data.Interface.Type.Diff


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
    = DiffValue (Named (Elem ValueDeclDiff ValueDecl))
    | DiffType (Named (Elem TypeDeclDiff TypeDecl))
    | SameReExport ExportName
    | DiffReExport (Elem' ExportName)
    deriving (Show, Eq, Ord)



data ValueDeclDiff = ValueDeclDiff TypeDiff (Change ValueDeclInfo)
    deriving (Show, Eq, Ord)

instance Diff ValueDecl ValueDeclDiff where
    diff (ValueDecl ta ia) (ValueDecl tb ib) =
        ValueDeclDiff (diff ta tb) (diff ia ib)

    toChange (ValueDeclDiff t i) = ValueDecl <$> toChange t <*> toChange i

    isChanged (ValueDeclDiff t i) = isChanged i || isChanged t



data TypeDeclDiff = TypeDeclDiff (Change Kind) (Change TypeDeclInfo)
    deriving (Show, Eq, Ord)

instance Diff TypeDecl TypeDeclDiff where
    diff (TypeDecl ka ia) (TypeDecl kb ib) =
        TypeDeclDiff (diff ka kb) (diff ia ib)

    toChange (TypeDeclDiff t i) = TypeDecl <$> toChange t <*> toChange i




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
        -- [] -> map (\vd -> DiffValue (rawName vd) . Removed $ unName vd)
        [] -> map (DiffValue . fmap Removed) (toList vds)
           ++ map (DiffType . fmap Removed) (toList tds)
           ++ map (DiffReExport . Removed) (toList res)
        e:es -> case e of
            LocalValue vd1
                | Just vd0 <- lookupName vd1 vds ->
                    DiffValue (named vd1 (Changed (on diff unName vd0 vd1)))
                        : go (deleteName vd0 vds, tds, res) es
                | otherwise ->
                    DiffValue (fmap Added vd1) : go (vds, tds, res) es
            LocalType td1
                | Just td0 <- lookupName td1 tds ->
                    DiffType (named td1 (Changed (on diff unName td0 td1)))
                        : go (vds, deleteName td0 tds, res) es
                | otherwise ->
                    DiffType (fmap Added td1) : go (vds, tds, res) es
            -- assume re-exports with the same name are equal
            ReExport n1
                | Just n0 <- lookupName n1 res ->
                    SameReExport n1 : go (vds, tds, deleteName n0 res) es
                | otherwise ->
                    DiffReExport (Added n1) : go (vds, tds, res) es


instance HasRawName ExportDiff where
    rawName = rawName . someName

instance HasNamespace ExportDiff where
    namespace = namespace . someName

instance HasSomeName ExportDiff where
    someName ed = case ed of
        DiffValue n -> SomeName Values (rawName n)
        DiffType n  -> SomeName Types (rawName n)
        SameReExport n -> someName n 
        DiffReExport e -> someName $ extractElem e
