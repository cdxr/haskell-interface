{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- temporary

module Data.Interface.ModuleDiff where

import Data.Function ( on )

import Data.Interface.Module
import Data.Interface.Change


-- | A record of all changes and non-changes to a `ModuleInterface`.
-- This contains enough information to recover the `ModuleInterface` from
-- before or after the changes.
--
data ModuleDiff = ModuleDiff
    { diffModuleName      :: !(DiffEq ModuleName)
    , diffModuleValues    :: !(DiffMap RawName ValueChange (Named ValueDecl))
    , diffModuleTypes     :: !(DiffMap RawName TypeChange (Named TypeDecl))
    , diffModuleReexports :: !(DiffSetEq (Qual SomeName))
    , diffModuleInstances :: !(DiffSetEq ClassInstance)
    }

deriving instance Show ModuleDiff


diffModules :: ModuleInterface -> ModuleInterface -> ModuleDiff
diffModules a b = ModuleDiff
    { diffModuleName      = on diffEq moduleName a b
    , diffModuleValues    = on diffMap moduleValues a b
    , diffModuleTypes     = on diffMap moduleTypes a b
    , diffModuleReexports = on diffSet moduleReexports a b
    , diffModuleInstances = on diffSet moduleInstances a b
    }


newtype ValueChange = ValueChange (Replace (Named ValueDecl))
    deriving (Show, Eq, Ord, Change (Named ValueDecl))

newtype TypeChange = TypeChange (Replace (Named TypeDecl))
    deriving (Show, Eq, Ord, Change (Named TypeDecl))
