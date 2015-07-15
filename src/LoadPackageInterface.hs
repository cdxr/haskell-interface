module LoadPackageInterface
(
    makePackageInterface

  , PackageEnv
  , newPackageEnv
  , setPackageDB
  , getPackageIndex
  , lookupPackageId

  -- * Cabal
  , PackageDB(..)
  , InstalledPackageId
  , InstalledPackageInfo
)
where

import Data.IORef

import GHC

import Distribution.Package as Cabal

import Distribution.Simple.PackageIndex ( InstalledPackageIndex, )
import qualified Distribution.Simple.PackageIndex as Cabal
import qualified Distribution.ModuleName as Cabal

-- for loading package databases:
import Data.List ( intercalate )
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
import Distribution.ModuleName ( components )
import Distribution.Simple.Program.Db as Cabal
import Distribution.Simple.GHC
import Distribution.Simple.Compiler ( PackageDB(..) )


import LoadModuleInterface

import Data.Interface.Module as Interface
import Data.Interface.Package as Interface


data PackageEnv = PackageEnv
    { programDB    :: Cabal.ProgramDb
    , packageDBVar :: IORef (Maybe PackageDB, InstalledPackageIndex)
    }

newPackageEnv :: IO PackageEnv
newPackageEnv = do
    progs <- configureAllKnownPrograms normal defaultProgramDb
    PackageEnv progs <$> newIORef (Nothing, Cabal.fromList [])

-- TODO: avoid additional work when this PackageDB is already loaded
setPackageDB :: PackageDB -> PackageEnv -> IO InstalledPackageIndex
setPackageDB db penv = do
    pix <- getPackageDBContents verbose db (programDB penv)
    atomicWriteIORef (packageDBVar penv) (Just db, pix)
    return pix

-- getPackageDB :: PackageEnv -> IO PackageDB

getPackageIndex :: PackageEnv -> IO InstalledPackageIndex 
getPackageIndex penv = snd <$> readIORef (packageDBVar penv)


lookupPackageId :: PackageEnv -> PackageId -> IO [InstalledPackageInfo]
lookupPackageId penv pid = do
    pkgIndex <- getPackageIndex penv
    pure $ Cabal.lookupSourcePackageId pkgIndex pid

--lookupPackageKey :: PackageEnv -> PackageKey -> IO InstalledPackageInfo


makeModuleName :: Cabal.ModuleName -> Interface.ModuleName
makeModuleName = intercalate "." . components

makePackageInterface :: InstalledPackageInfo -> IO PackageInterface
makePackageInterface ipi = withGhc $ do
    let pkgKey = packageKey ipi
    exposed <- makeModuleEnv pkgKey $ map exposedName (exposedModules ipi)
    hidden  <- makeModuleEnv pkgKey $ hiddenModules ipi
    pure $ PackageInterface
        { pkgId             = sourcePackageId ipi
        , pkgExposedModules = exposed
        , pkgHiddenModules  = hidden
        }

makeModuleEnv :: Cabal.PackageKey -> [Cabal.ModuleName] -> Ghc ModuleEnv
makeModuleEnv k =
    fmap (foldMap singleModuleInterface) . mapM loadInterface
  where
    loadInterface :: Cabal.ModuleName -> Ghc ModuleInterface
    loadInterface modName =
        makeInterface $ ModuleGoal (makeModuleName modName)
                                   (Just $ pkgKeyFromCabal k)


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

