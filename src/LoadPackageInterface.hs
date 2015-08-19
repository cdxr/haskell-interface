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

makePackageInterface
    :: (String -> IO ())        -- ^ error printer
    -> LocPackage               -- ^ package location
    -> IO PackageInterface      -- ^ package interface
makePackageInterface debugInfo lp@(LocPackage _ ipi) = do
    liftIO . debugInfo $
        "starting fresh GHC session to load " ++ showLocPackage lp

    withGhc $ do
        ghcLocPackage lp

        liftIO $ debugInfo "package loaded"

        let pkgKey = packageKey ipi
        exposed <- makeModuleEnv pkgKey $ map exposedName (exposedModules ipi)
        let hidden = mempty -- TODO
        pure $ PackageInterface
            { pkgId             = sourcePackageId ipi
            , pkgExposedModules = exposed
            , pkgHiddenModules  = hidden
            }

-- | Expose a single package to GHC. Invoking `ghcLocPackage` again overwrites
-- the effects of previous calls.
ghcLocPackage :: LocPackage -> Ghc ()
ghcLocPackage (LocPackage db ipi) = do
    {- Workaround  (TODO)

        This is a temporary hack to work around some confusing GHC behavior.
        When the given package database is either the global or the user
        package database, the equivalent of a "-package-key" flag is used to
        ensure that the package is visible. This is done with the
        `exposePackage` function defined below. If `exposePackage` was not
        used for the global and user databases, then a package would only be
        visible if there was not a more recent version of that package in the
        database.

        `exposePackage` *should* also be applied when given a path to a
        specific package database (for example when the package is in a
        sandbox).  However, doing so consistently results in the error
        "cannot satisfy -package-key PACKAGE_KEY". As a temporary workaround,
        the "-package-key" flag is not provided in this case. However, this
        means that when multiple versions of a package are installed in a
        sandbox only one version will work, and the others will result in the
        above error.
    -}
    dflags0 <- getSessionDynFlags
    let dflags = case db of
            GlobalPackageDB -> exposePackage dflags0
            UserPackageDB -> exposePackage dflags0
            SpecificPackageDB path -> addPackagePath path dflags0

    _pkgKeys <- setSessionDynFlags dflags

    return ()
  where
    exposePackage dflags =
        let pflag = ExposePackage (pkgArg ipi) noRenaming
        in dflags { packageFlags = pflag : packageFlags dflags }

    addPackagePath path dflags =
        dflags { extraPkgConfs = (PkgConfFile path :) }

    pkgArg :: InstalledPackageInfo -> PackageArg
    pkgArg = PackageKeyArg . Cabal.display . packageKey

    noRenaming = ModRenaming True []


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

