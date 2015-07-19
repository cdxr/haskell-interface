module LoadPackageInterface
(
    makePackageInterface

  , PackageEnv
  , newPackageEnv
  , resetPackageDB
  , getPackageIndex

  , PackageFilter(..)
  , readPackageFilter
  , showPackageFilter
  , filterPackages

  , LocPackage(..)
  , packageLocations 

  -- * Cabal
  , PackageDB(..)
  , InstalledPackageId
  , InstalledPackageInfo
)
where

import Data.IORef
import Data.Maybe ( maybeToList )
import Data.Map ( Map )
import qualified Data.Map as Map

import Control.Monad.IO.Class

import GHC
import DynFlags
import qualified Outputable as Out
import qualified Packages as GHC
import qualified PackageConfig as GHC

import Distribution.Text as Cabal
import Distribution.Package as Cabal

import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
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
    , packageDBRef :: IORef (Map PackageDB InstalledPackageIndex)
    }

newPackageEnv :: IO PackageEnv
newPackageEnv = do
    progs <- configureAllKnownPrograms normal defaultProgramDb
    PackageEnv progs <$> newIORef Map.empty

resetPackageDB :: PackageEnv -> PackageDB -> IO ()
resetPackageDB env pdb =
    modifyIORef (packageDBRef env) $ Map.delete pdb

getPackageIndex :: PackageEnv -> PackageDB -> IO InstalledPackageIndex
getPackageIndex env pdb = do
    m <- readIORef (packageDBRef env)
    case Map.lookup pdb m of
        Just ipi -> pure ipi
        Nothing -> do
            ipi <- getPackageDBContents silent pdb (programDB env)
            atomicWriteIORef (packageDBRef env) $ Map.insert pdb ipi m
            pure ipi


data PackageFilter
    = MatchName PackageName
    | MatchId PackageId
    | MatchInstalledId InstalledPackageId
    deriving (Show, Eq, Ord)

readPackageFilter :: String -> PackageFilter
readPackageFilter s
    | '-' `elem` s, Just pid <- parsePackageId s =
        MatchId pid
    | otherwise =
        MatchName $ PackageName s

showPackageFilter :: PackageFilter -> String
showPackageFilter pf = case pf of
    MatchName n -> Cabal.display n
    MatchId pid -> Cabal.display pid
    MatchInstalledId ipid -> Cabal.display ipid


filterPackages ::
    PackageFilter -> InstalledPackageIndex -> [InstalledPackageInfo]
filterPackages pf pix = case pf of
    MatchName n -> concatMap snd $ Cabal.lookupPackageName pix n
    MatchId pid -> Cabal.lookupSourcePackageId pix pid 
    MatchInstalledId ipid ->
        maybeToList $ Cabal.lookupInstalledPackageId pix ipid


-- | A package annotated with the `PackageDB` where it is located.
data LocPackage = LocPackage PackageDB InstalledPackageInfo
    deriving (Show)

packageLocations ::
    PackageEnv ->
    PackageFilter ->
    [PackageDB] ->
    IO [LocPackage]
packageLocations env pf = fmap concat . mapM list
  where
    list :: PackageDB -> IO [LocPackage]
    list db = do
        pix <- getPackageIndex env db 
        pure [ LocPackage db pkg | pkg <- filterPackages pf pix ]


makeModuleName :: Cabal.ModuleName -> Interface.ModuleName
makeModuleName = intercalate "." . components

makePackageInterface :: LocPackage -> IO PackageInterface
makePackageInterface lp@(LocPackage db ipi) = do
    mapM_ (print . exposedName) (exposedModules ipi)
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
    _ <- setSessionDynFlags $ dflags0
            { extraPkgConfs = (toPkgConfRef db :)
            , packageFlags = [ ExposePackage (pkgKeyArg ipi) modRenaming ]
            }
    _ <- liftIO $ GHC.initPackages dflags0
        -- ignored value: ^ do we need to set the session flags again?

    debugPackageFlags
    
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

