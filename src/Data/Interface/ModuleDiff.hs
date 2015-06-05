module Data.Interface.ModuleDiff where

import Data.Interface.Module
import Data.Interface.Change

import Data.Function ( on )
import Data.Map ( Map )


-- | A record of changes to a `ModuleInterface`. This contains enough
-- information to recover the `ModuleInterface` from before or after the
-- changes.
--
data ModuleDiff = ModuleDiff
    { diffModuleName      :: !(Diff ModuleName)
        -- ^ possibly-changed ModuleName
    , diffModuleDecls     :: !(Map DeclName (Diff Decl))
        -- ^ map from names to changes
    , diffModuleReexports :: [Diff QualName]
    , diffModuleInstances :: [Diff ClassInstance]
    } deriving (Show)


moduleDiff :: ModuleInterface -> ModuleInterface -> ModuleDiff
moduleDiff a b = ModuleDiff
    { diffModuleName      = on diffSpan moduleName a b
    , diffModuleDecls     = on diffMapEq moduleDecls a b
    , diffModuleReexports = on diffSet moduleReexports a b
    , diffModuleInstances = on diffSet moduleInstances a b
    }
