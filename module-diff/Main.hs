module Main where

import Control.Monad.IO.Class

import LoadModuleInterface
import LoadPackageInterface

import Data.Interface

import Task
import Program
import ProgramArgs
import Html

import qualified Console


main :: IO ()
main = runProgram start =<< parseProgramArgs


start :: Program ()
start = do
    whenFlag_ outputClassInstances $
        error "--instances: unimplemented"

    format <- getArg outputFormat
    output format =<< loadTask =<< getArg programTask


output :: OutputFormat -> LoadedTask -> Program ()
output f = case f of
    OutputConsole -> Console.runTask
    OutputHtml -> Html.runTask


loadTask :: TargetTask -> Program LoadedTask
loadTask = bitraverseTask loadPackage $ \mt -> (,) mt <$> loadModule mt


loadPackage :: PackageTarget -> Program PackageInterface
loadPackage (PackageTarget pkgFilter dbs) = do
    liftIO $ print pkgFilter
    lps <- withPackageEnv $ \env -> packageLocations env pkgFilter dbs
    case lps of
        [] -> error $ "could not find package: " ++ showPackageFilter pkgFilter
        lp:_ -> liftIO $ makePackageInterface lp


loadModule :: ModuleTarget -> Program ModuleInterface
loadModule t = do
    iface <- liftIO $ withGhc $ makeInterface =<< guessGoal t

    -- don't qualify names from this module
    unqualify $ moduleName iface  

    ms <- getArg hideString
    pure $ case ms of
        Nothing -> iface
        Just n -> filterInterfaceNames (/= n) iface


-- Incomplete "internal" features:

{-
-- | This function is horribly inefficient, but is only used for processing
-- "test modules". A test module is a module containing exports with names
-- suffixed with "_0" and "_1", which are compared as if they originated in
-- different module versions.
runTestModule :: ModuleTarget -> Program ()
runTestModule t = do
    iface <- loadModule t

    let iface0 = refineInterface "_0" (not . belong1) iface
        iface1 = refineInterface "_1" (not . belong0) iface

    printModuleInterface iface0
    printModuleInterface iface1

    printModuleDiff (t ++ "_0", t ++ "_1") $
        diff iface0 iface1

  where
    refineInterface ::
        String ->             -- remove this suffix from names
        (RawName -> Bool) ->  -- filter everything using this predicate
        ModuleInterface -> ModuleInterface
    refineInterface suff p = removeSuffix suff . filterInterfaceNames p

    splitLast2 s = case splitAt 2 (reverse s) of
                    (a, b) -> (reverse b, reverse a)
    last2 = snd . splitLast2

    belong0, belong1 :: String -> Bool
    belong0 = (==) "_0" . last2
    belong1 = (==) "_1" . last2

    removeSuffix :: String -> ModuleInterface -> ModuleInterface
    removeSuffix suffix =
        renameAll $ \s -> case splitLast2 s of
            (pref, suf)
                | suf == suffix -> pref
                | otherwise -> s
-}
