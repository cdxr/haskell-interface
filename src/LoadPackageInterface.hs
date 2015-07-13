module LoadPackageInterface where

import GHC
import FastString as GHC
import qualified Module as GHC
import PackageConfig as GHC

import GHC.PackageDb as GhcPkg

import Data.List ( intercalate, find )

import Distribution.Text as Cabal
import Distribution.Package as Cabal
import qualified Distribution.InstalledPackageInfo as Cabal
import Distribution.Simple.PackageIndex
import qualified Distribution.ModuleName as Cabal

import LoadModuleInterface

import Data.Interface.Module ( ModuleInterface )
import Data.Interface.Package as Interface


newtype PackageDB = PackageDB [GHC.PackageConfig]

loadPackageDB :: FilePath -> IO PackageDB
loadPackageDB = fmap PackageDB . GhcPkg.readPackageDbForGhc

findPackage ::
    (GHC.PackageConfig -> Bool) -> PackageDB -> Maybe GHC.PackageConfig
findPackage p (PackageDB cs) = find p cs

lookupSourcePackageId' ::
    GHC.SourcePackageId -> PackageDB -> Maybe GHC.PackageConfig
lookupSourcePackageId' spid = findPackage $ (== spid) . sourcePackageId

lookupPackageId ::
    Interface.PackageId -> PackageDB -> Maybe GHC.PackageConfig
lookupPackageId (PackageId s) =
    lookupSourcePackageId' . GHC.SourcePackageId $ mkFastString s


makePackageId :: GHC.SourcePackageId -> Interface.PackageId
makePackageId (GHC.SourcePackageId fs) = PackageId $ unpackFS fs


makePackageInterface :: GHC.PackageConfig -> IO PackageInterface
makePackageInterface ipi = withGhc $ do
    let pkgKey = packageKey ipi
    exposed <- makeModuleEnv pkgKey $ map exposedName (exposedModules ipi)
    hidden  <- makeModuleEnv pkgKey $ hiddenModules ipi
    pure $ PackageInterface
        { pkgId             = makePackageId (sourcePackageId ipi)
        , pkgExposedModules = exposed
        , pkgHiddenModules  = hidden
        }

makeModuleEnv :: GHC.PackageKey -> [GHC.ModuleName] -> Ghc ModuleEnv
makeModuleEnv pkgKey =
    fmap (foldMap singleModuleInterface) . mapM loadInterface
  where
    loadInterface :: GHC.ModuleName -> Ghc ModuleInterface
    loadInterface modName = makeInterface $ ModuleGoal modName (Just pkgKey)
    

{- TODO:
data PackageGoal
    = IdGoal PackageId
    | InstalledIdGoal InstalledPackageId
    deriving (Show, Eq, Ord)
-}

{-
parsePackageId :: String -> Maybe PackageId
parsePackageId = Cabal.simpleParse

-- | Construct an interface for the package with the given `PackageId`. If
-- there are multiple matching packages, one of them is selected arbitrarily.
packageIdInterface ::
    PackageId -> InstalledPackageIndex -> IO (Maybe PackageInterface)
packageIdInterface pid ipix =
    case lookupSourcePackageId ipix pid of
        [] -> pure Nothing
        (ipi:_) -> Just <$> makePackageInterface ipi


loadPackageInterface ::
    Cabal.InstalledPackageId ->
    InstalledPackageIndex ->
    IO (Maybe PackageInterface)
loadPackageInterface ipid ipix =
    case lookupInstalledPackageId ipix ipid of
        Nothing -> pure Nothing
        Just ipi -> Just <$> makePackageInterface ipi
-}


{-
makePackageInfo :: Cabal.InstalledPackageInfo -> PackageInfo
makePackageInfo ipi = PackageInfo
    { pkgLicense = Cabal.license ipi
    , pkgCopyright = Cabal.copyright ipi
    , pkgMaintainer = Cabal.maintainer ipi
    }
-}

