module LoadPackageInterface
(
    makePackageInterface

  , PackageEnv
  , newPackageEnv
  , resetPackageDB
  , getPackageIndex

  , PackageSelector(..)
  , readPackageSelector
  , showPackageSelector
  , filterPackages

  , LocPackage(..)
  , showLocPackage
  , packageLocations 

  -- * Cabal
  , PackageDB(..)
  , InstalledPackageId
  , InstalledPackageInfo
)
where

import Control.Monad.IO.Class

import GHC
import DynFlags
import qualified Outputable as Out
import qualified Packages as GHC

import Distribution.Text as Cabal
import Distribution.Package as Cabal

import qualified Distribution.ModuleName as Cabal

-- for loading package databases:
import Data.List ( intercalate )
import Distribution.InstalledPackageInfo
import Distribution.ModuleName ( components )
import Distribution.Simple.Compiler ( PackageDB(..) )

import LoadModuleInterface

import Data.Interface.Module as Interface
import Data.Interface.Package as Interface

import Data.Interface.GHC.Package


makeModuleName :: Cabal.ModuleName -> Interface.ModuleName
makeModuleName = intercalate "." . components

makePackageInterface :: LocPackage -> IO PackageInterface
makePackageInterface lp@(LocPackage _ ipi) = do
    withGhc $ do
        ghcLocPackage lp

        let pkgKey = packageKey ipi
        exposed <- makeModuleEnv pkgKey $ map exposedName (exposedModules ipi)
        hidden  <- makeModuleEnv pkgKey $ hiddenModules ipi
        pure $ PackageInterface
            { pkgId             = sourcePackageId ipi
            , pkgExposedModules = exposed
            , pkgHiddenModules  = hidden
            }

ghcLocPackage :: LocPackage -> Ghc ()
ghcLocPackage (LocPackage db ipi) = do
    dflags0 <- getSessionDynFlags
    _pkgKeys <- setSessionDynFlags dflags0
            { extraPkgConfs = (toPkgConfRef db :)
            , packageFlags = [ ExposePackage (pkgKeyArg ipi) modRenaming ]
            }
    --liftIO $ GHC.linkPackages dflags pkgKeys

    return ()
  where
    toPkgConfRef :: PackageDB -> PkgConfRef
    toPkgConfRef db = case db of
        GlobalPackageDB      -> GlobalPkgConf
        UserPackageDB        -> UserPkgConf
        SpecificPackageDB fp -> PkgConfFile fp

    pkgKeyArg :: InstalledPackageInfo -> PackageArg
    pkgKeyArg = PackageKeyArg . Cabal.display . packageKey

    modRenaming = ModRenaming False []  -- TODO


debugPackageFlags :: Ghc ()
debugPackageFlags = do
    dynFlags <- getSessionDynFlags
    let output s f = putStrLn $ "  " ++ s ++ ": " ++ show (f dynFlags)
    liftIO $ do
        putStrLn "debugPackageFlags:"
        output "packageFlags" packageFlags
        output "pkgDatabase" $
            (fmap.map) (showInstalledPkgInfo dynFlags) . pkgDatabase
  where
    showInstalledPkgInfo dfs =
        Out.showSDoc dfs . Out.ppr . GHC.installedPackageId


makeModuleEnv :: Cabal.PackageKey -> [Cabal.ModuleName] -> Ghc ModuleEnv
makeModuleEnv k =
    fmap (foldMap singleModuleInterface) . mapM loadInterface
  where
    loadInterface :: Cabal.ModuleName -> Ghc ModuleInterface
    loadInterface modName =
        makeInterface $ ModuleGoal (makeModuleName modName)
                                   (Just $ pkgKeyFromCabal k)


{-
makePackageInfo :: Cabal.InstalledPackageInfo -> PackageInfo
makePackageInfo ipi = PackageInfo
    { pkgLicense = Cabal.license ipi
    , pkgCopyright = Cabal.copyright ipi
    , pkgMaintainer = Cabal.maintainer ipi
    }
-}

