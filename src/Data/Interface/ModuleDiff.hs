{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interface.ModuleDiff where

import qualified Data.Set as Set ( fromList )
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
    { diffModuleName       :: !(Change ModuleName)
    , diffModuleTypeCons   :: !(DiffMapEq RawName TypeCon)
    , diffModuleValueDecls :: !(DiffMap RawName ValueDeclChange (Named ValueDecl))
    , diffModuleTypeDecls  :: !(DiffMap RawName TypeDeclChange (Named TypeDecl))
    , diffModuleExportList :: !(DiffSetEq ExportName)
    , diffModuleInstances  :: !(DiffSetEq ClassInstance)
    } deriving (Show)


diffModules :: ModuleInterface -> ModuleInterface -> ModuleDiff
diffModules a b = ModuleDiff
    { diffModuleName       = on diff moduleName a b
    , diffModuleTypeCons   = on diffMap moduleTypeCons a b
    , diffModuleValueDecls = on diffMap moduleValueDecls a b
    , diffModuleTypeDecls  = on diffMap moduleTypeDecls a b
    , diffModuleExportList = on diffSet (Set.fromList . moduleExportList) a b
    , diffModuleInstances  = on diffSet moduleInstances a b
    }


type ValueDeclChange = Change (Named ValueDecl)


type TypeDeclChange = Change (Named TypeDecl)
