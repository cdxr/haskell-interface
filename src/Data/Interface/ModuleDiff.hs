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
    { diffModuleName      :: !(Diff Change ModuleName)
    , diffModuleValues    :: !(Map ValueName (Diff ElemChange ValueDecl))
    , diffModuleTypes     :: !(Map TypeName (Diff ElemChange TypeDecl))
    , diffModuleReexports :: [Diff ElemChange (Qual SomeName)]
    , diffModuleInstances :: [Diff ElemChange ClassInstance]
    } deriving (Show)


moduleDiff :: ModuleInterface -> ModuleInterface -> ModuleDiff
moduleDiff a b = ModuleDiff
    { diffModuleName      = on diffEq moduleName a b
    , diffModuleValues    = on (diffMap declDiff) moduleValues a b
    , diffModuleTypes     = on (diffMap declDiff) moduleTypes a b
    , diffModuleReexports = on diffSet moduleReexports a b
    , diffModuleInstances = on diffSet moduleInstances a b
    }


declDiff :: Name s -> Decl s -> Decl s -> Diff Change (Decl s)
declDiff _ = diffEq


-- | A GADT enumeration of each type of element found in a ModuleInterface
data ModuleElem a where
    Decl'     :: ModuleElem (Decl s)
    Reexport' :: ModuleElem (Qual SomeName)
    Instance' :: ModuleElem ClassInstance

{- ModuleElem notes:
     - ADiff and AChange can be parameterized over ModuleElem to produce a
       sum type representing a single module diff/change
-}


elemDiffs :: ModuleDiff -> [ADiff ElemChange ModuleElem]
elemDiffs mdiff =
    concat $
        [ TagF Decl' <$> (Map.elems $ diffModuleValues mdiff)
        , TagF Decl' <$> (Map.elems $ diffModuleTypes mdiff)
        , TagF Reexport' <$> (diffModuleReexports mdiff)
        , TagF Instance' <$> (diffModuleInstances mdiff)
        ]
    

elemChanges :: ModuleDiff -> [AnElemChange ModuleElem]
elemChanges = mapMaybe maybeAChange . elemDiffs


instance Show (ModuleElem a) where
    showsPrec _ t = showString $ case t of
        Decl'     -> "Decl'"
        Reexport' -> "Reexport'"
        Instance' -> "Instance'"

instance Show1 ModuleElem where showsPrec1 = showsPrec
