module Console where

import Control.Monad.IO.Class

import Data.Interface
import Data.Interface.Change ( diff )

import Task
import Program
import Render ( Render(..), renderStdout )


runTask :: LoadedTask -> Program ()
runTask task = case task of
    PrintPackage p -> printPackageInterface p
    PrintModule (_, m) -> printModuleInterface m
    CompareModules (t0, m0) (t1, m1) -> printModuleDiff (t0, t1) (diff m0 m1)
    --RunTestModule t -> runTestModule t


printPackageInterface :: PackageInterface -> Program ()
printPackageInterface iface = do
    outputLine $ unlines
        [ "\n*** Package: ***\n"
        , showPackageId iface
        , "\nExposed modules:"
        ]

    mapM_ render (pkgExposedModules iface)

printModuleInterface :: ModuleInterface -> Program ()
printModuleInterface iface = do
    outputLine $ unlines
        [ "\n*** Module: " ++ moduleName iface ++ " ***\n"
        , "Exposed type constructors:\n"
        ]

    mapM_ render (moduleTypeCons iface)

    outputLine "Module exports:\n"

    mapM_ render (compileModuleExports iface)


printModuleDiff :: (ModuleTarget, ModuleTarget) -> ModuleDiff -> Program ()
printModuleDiff (t0, t1) mdiff = do
    outputLine $ unlines
        [ ""
        , "************************************"
        , " Comparing Modules:"
        , "   " ++ t0
        , "   " ++ t1
        , "************************************"
        ]

    mapM_ render (diffModuleExports mdiff)


outputLine :: String -> Program ()
outputLine = liftIO . putStrLn


render :: (Render a) => a -> Program ()
render a = do
    qc <- getQualContext
    liftIO $ renderStdout 2 qc a
    outputLine ""


