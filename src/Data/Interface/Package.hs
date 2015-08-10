{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interface.Package
(
    ModuleEnv
  , singleModuleInterface

  , PackageInterface(..)
  , showPackageId
  , PackageDiff(..)

  , PackageId
  , PackageIdentifier
  , parsePackageId
)
where

import Data.Function ( on )
import Data.Map ( Map )
import qualified Data.Map as Map

import Distribution.Text
import Distribution.Package as C
--import qualified Distribution.License as C

import Data.Interface.Change

import Data.Interface.Module


type ModuleEnv = Map ModuleName ModuleInterface

singleModuleInterface :: ModuleInterface -> ModuleEnv
singleModuleInterface iface = Map.singleton (moduleName iface) iface


data PackageInterface = PackageInterface
    { pkgId             :: PackageId
    --, pkgInfo           :: PackageInfo
    , pkgExposedModules :: ModuleEnv
    , pkgHiddenModules  :: ModuleEnv
    } deriving (Show)

{- PackageInterface notes:
     - PackageInterface provides a view of an installed package after
       conditionals and dependency ranges have been resolved.
     - Hidden modules must be included in the map because their exports can be
       visible in exposed modules.
-}

showPackageId :: PackageId -> String
showPackageId = display


data PackageDiff = PackageDiff
    { diffPkgId             :: Change C.PackageId
    --, diffPkgInfo           :: Change PackageInfo
    , diffPkgExposedModules :: MapDiff ModuleName ModuleDiff ModuleInterface
    , diffPkgHiddenModules  :: MapDiff ModuleName ModuleDiff ModuleInterface
    }

instance Diff PackageInterface PackageDiff where
    diff a b = PackageDiff
        { diffPkgId             = on diff pkgId a b
        --, diffPkgInfo           = on diff pkgInfo a b
        , diffPkgExposedModules = on diff pkgExposedModules a b
        , diffPkgHiddenModules  = on diff pkgHiddenModules a b
        }

    toChange d =
        PackageInterface
            <$> toChange (diffPkgId d)
            <*> toChange (diffPkgExposedModules d)
            <*> toChange (diffPkgHiddenModules d)



parsePackageId :: String -> Maybe PackageId
parsePackageId = simpleParse



{-- TODO
 - Should we include this information?
data PackageInfo = PackageInfo
    { pkgLicense    :: C.License
    , pkgCopyright  :: String
    , pkgMaintainer :: String
    -- ... etc
    } deriving (Show, Eq)
-}
 

