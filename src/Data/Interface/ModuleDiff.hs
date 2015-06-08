{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Data.Interface.ModuleDiff where

import Data.Functor.Classes

import Data.Function ( on )
import Data.Maybe ( mapMaybe )

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Interface.Module
import Data.Interface.Change


-- | A record of all changes and non-changes to a `ModuleInterface`.
-- This contains enough information to recover the `ModuleInterface` from
-- before or after the changes.
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


-- | A GADT enumeration of each type of element found in a ModuleInterface
data ModuleElem a where
    Name'     :: ModuleElem ModuleName
    Decl'     :: ModuleElem Decl
    Reexport' :: ModuleElem QualName
    Instance' :: ModuleElem ClassInstance


elemDiffs :: ModuleDiff -> [ADiff ModuleElem]
elemDiffs mdiff =
    concat $
        [ ADiff Name' <$> [diffModuleName mdiff]
        , ADiff Decl' <$> (Map.elems $ diffModuleDecls mdiff)
        , ADiff Reexport' <$> (diffModuleReexports mdiff)
        , ADiff Instance' <$> (diffModuleInstances mdiff)
        ]

elemChanges :: ModuleDiff -> [AChange ModuleElem]
elemChanges = mapMaybe maybeAChange . elemDiffs


instance Show (ModuleElem a) where
    showsPrec _ t = showString $ case t of
        Name' -> "Name'"
        Decl' -> "Decl'"
        Reexport' -> "Reexport'"
        Instance' -> "Instance'"

instance Show1 ModuleElem where showsPrec1 = showsPrec
