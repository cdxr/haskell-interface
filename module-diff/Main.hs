module Main where

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe ( isJust )

import LoadModuleInterface
import LoadPackageInterface

import Data.Interface
import Data.Interface.Change ( diff )

import Task
import Program
import ProgramArgs
import Html

import qualified Console


main :: IO ()
main = runProgram start =<< parseProgramArgs


start :: Program ()
start = do
    whenArgs_ outputClassInstances $
        error "--instances: currently disabled"

    format <- getArg outputFormat
    output format =<< loadTarget =<< resolveProgramTarget


output :: OutputFormat -> ProgramResult -> Program ()
output f = case f of
    OutputConsole -> Console.runTask
    OutputHtml -> Html.runTask


loadTarget :: ProgramTarget -> Program ProgramResult
loadTarget t = case t of
    PrintPackage pt ->
        APackage <$> loadPackage pt
    ComparePackages pt0 pt1 ->
        APackageDiff <$> (diff <$> loadPackage pt0 <*> loadPackage pt1)
    PrintModule mt ->
        AModule mt <$> loadModule mt
    CompareModules mt0 mt1 ->
        AModuleDiff mt0 mt1 <$> (diff <$> loadModule mt0 <*> loadModule mt1)
    


loadPackage :: PackageTarget -> Program PackageInterface
loadPackage target@(Target mdb sel) = do
    dbStack <- case mdb of
        Nothing -> getDefaultDBStack
        Just db -> pure [db]

    verbose $ "loading package target: " ++ showPackageSelector sel
           ++ "\n using DB stack: " ++ show dbStack

    v <- (== Verbose) <$> getArg verbosity
    lps <- withPackageEnv $ \env -> packageLocations v env sel dbStack
    case lps of
        [] -> error $ "could not find package: " ++ showPackageSelector sel
        lp:_ -> do
            f <- getVerbosePrinter 
            liftIO $ makePackageInterface f lp


loadModule :: ModuleTarget -> Program ModuleInterface
loadModule (Target mpath s) = do
    when (isJust mpath) $
        error "--package-db unimplemented for module targets"   -- TODO

    iface <- liftIO $ withGhc $
        makeInterface =<< guessGoal (either id id s)
            -- TODO: ^ we discard the information about whether the module is
            --         a path or module name, and let GHC work it out by itself

    -- remember not to qualify names from this module
    unqualify $ moduleName iface  

    pure iface
