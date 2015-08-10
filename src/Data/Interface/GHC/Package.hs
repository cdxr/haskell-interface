module Data.Interface.GHC.Package where

import Data.IORef
import Data.Maybe ( maybeToList )
import Data.Map ( Map )
import qualified Data.Map as Map

import Distribution.Text as Cabal
import Distribution.Package as Cabal

import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import qualified Distribution.Simple.PackageIndex as Cabal

-- for loading package databases:
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
import Distribution.Simple.Program.Db as Cabal
import Distribution.Simple.GHC
import Distribution.Simple.Compiler ( PackageDB(..) )

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


data PackageSelector
    = MatchName PackageName
    | MatchId PackageId
    | MatchInstalledId InstalledPackageId
    deriving (Show, Eq, Ord)

-- TODO: identify `InstalledPackageId`s
readPackageSelector :: String -> PackageSelector
readPackageSelector s
    | '-' `elem` s, Just pid <- parsePackageId s =
        MatchId pid
    | otherwise =
        MatchName $ PackageName s

showPackageSelector :: PackageSelector -> String
showPackageSelector sel = case sel of
    MatchName n -> Cabal.display n
    MatchId pid -> Cabal.display pid
    MatchInstalledId ipid -> Cabal.display ipid


filterPackages ::
    PackageSelector -> InstalledPackageIndex -> [InstalledPackageInfo]
filterPackages sel pix = case sel of
    MatchName n -> concatMap snd $ Cabal.lookupPackageName pix n
    MatchId pid -> Cabal.lookupSourcePackageId pix pid 
    MatchInstalledId ipid ->
        maybeToList $ Cabal.lookupInstalledPackageId pix ipid


-- | A package annotated with the `PackageDB` where it is located.
data LocPackage = LocPackage PackageDB InstalledPackageInfo
    deriving (Show)

showLocPackage :: LocPackage -> String
showLocPackage (LocPackage db ipi) =
    Cabal.display (Cabal.packageId ipi) ++ " " ++ case db of
        GlobalPackageDB -> "[global]"
        UserPackageDB -> "[user]"
        SpecificPackageDB fp -> "(" ++ fp ++ ")"

packageLocations ::
    PackageEnv ->
    PackageSelector ->
    [PackageDB] ->
    IO [LocPackage]
packageLocations env sel = fmap concat . mapM list
  where
    list :: PackageDB -> IO [LocPackage]
    list db = do
        pix <- getPackageIndex env db 
        pure [ LocPackage db pkg | pkg <- filterPackages sel pix ]

