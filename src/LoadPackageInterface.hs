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
        --hidden  <- makeModuleEnv pkgKey $ hiddenModules ipi
        let hidden = mempty
        pure $ PackageInterface
            { pkgId             = sourcePackageId ipi
            , pkgExposedModules = exposed
            , pkgHiddenModules  = hidden
            }

-- | Expose a package to GHC
ghcLocPackage :: LocPackage -> Ghc ()
ghcLocPackage (LocPackage db ipi) = do
    dflags0 <- getSessionDynFlags
    pkgKeys <- setSessionDynFlags dflags0
            { extraPkgConfs = (toPkgConfRef db :)
            , packageFlags = [ ExposePackage (pkgArg ipi) modRenaming ]
            }
    --liftIO $ GHC.linkPackages dflags0 pkgKeys

    return ()
  where
    toPkgConfRef :: PackageDB -> PkgConfRef
    toPkgConfRef db = case db of
        GlobalPackageDB      -> GlobalPkgConf
        UserPackageDB        -> UserPkgConf
        SpecificPackageDB fp -> PkgConfFile fp

    pkgArg :: InstalledPackageInfo -> PackageArg
    pkgArg = PackageKeyArg . Cabal.display . packageKey
    --pkgArg = PackageIdArg . Cabal.display . Cabal.installedPackageId

    modRenaming = ModRenaming True []


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

