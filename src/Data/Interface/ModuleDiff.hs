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
    { diffModuleName       :: !(DiffEq ModuleName)
    , diffModuleTypeCons   :: !(DiffMapEq RawName TypeCon)
    , diffModuleValueDecls :: !(DiffMap RawName ValueDeclChange (Named ValueDecl))
    , diffModuleTypeDecls  :: !(DiffMap RawName TypeDeclChange (Named TypeDecl))
    , diffModuleExportList :: !(DiffSetEq ExportName)
    , diffModuleInstances  :: !(DiffSetEq ClassInstance)
    } deriving (Show)


diffModules :: ModuleInterface -> ModuleInterface -> ModuleDiff
diffModules a b = ModuleDiff
    { diffModuleName       = on diffEq moduleName a b
    , diffModuleTypeCons   = on diffMap moduleTypeCons a b
    , diffModuleValueDecls = on diffMap moduleValueDecls a b
    , diffModuleTypeDecls  = on diffMap moduleTypeDecls a b
    , diffModuleExportList = on diffSet (Set.fromList . moduleExportList) a b
    , diffModuleInstances  = on diffSet moduleInstances a b
    }


-- TODO: the definitions of ValueChange and TypeChange are temporary

newtype ValueDeclChange = ValueDeclChange (Replace (Named ValueDecl))
    deriving (Show, Eq, Ord)

instance Change (Named ValueDecl) ValueDeclChange where
    change a b
        | namedThing a == namedThing b = Just $ ValueDeclChange $ Replace a b
        | otherwise = Nothing


newtype TypeDeclChange = TypeDeclChange (Replace (Named TypeDecl))
    deriving (Show, Eq, Ord)

instance Change (Named TypeDecl) TypeDeclChange where
    change a b
        | namedThing a == namedThing b = Just $ TypeDeclChange $ Replace a b
        | otherwise = Nothing
