{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Interface.Package where

import Data.Function ( on )
import Data.Map ( Map )
import qualified Data.Map as Map

import qualified Distribution.Package as C
import qualified Distribution.License as C

import Data.Interface.Name ( ModuleName )
import Data.Interface.Change
import Data.Interface.Module
import Data.Interface.ModuleDiff


data PackageInfo = PackageInfo
    { pkgLicense    :: C.License
    , pkgCopyright  :: String
    , pkgMaintainer :: String
    -- ... TODO etc
    } deriving (Show, Eq)

data PackageInterface = PackageInterface
    { pkgId      :: C.PackageId
    , pkgInfo    :: PackageInfo
    , pkgModules :: Map ModuleName ModuleInterface
    , pkgExposedModules :: [ModuleName]
    } deriving (Show)

{- PackageInterface notes:
     - PackageInterface provides a view of an installed package after
       conditionals and dependency ranges have been resolved.
     - Hidden modules must be included in the map because their exports can be
       visible in exposed modules.
-}


data PackageDiff = PackageDiff
    { diffPkgId             :: Change C.PackageId
    , diffPkgInfo           :: Change PackageInfo
    , diffPkgModules        :: MapDiff ModuleName ModuleDiff ModuleInterface
    , diffPkgExposedModules :: Change [ModuleName]
    }

instance Diff PackageInterface PackageDiff where
    diff a b = PackageDiff
        { diffPkgId             = on diff pkgId a b
        , diffPkgInfo           = on diff pkgInfo a b
        , diffPkgModules        = on diff pkgModules a b
        , diffPkgExposedModules = on diff pkgExposedModules a b
        }
