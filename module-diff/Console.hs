module Console where

import Control.Monad.IO.Class

import Data.Interface

import Task
import Program
import Render ( Render(..), renderStdout )


runTask :: ProgramResult -> Program ()
runTask r = case r of
    APackage p -> printPackageInterface p
    AModule _ m -> printModuleInterface m
    AModuleDiff t0 t1 mdiff -> printModuleDiff (t0, t1) mdiff
    _ -> error "Console.runTask unimplemented for this target type"


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
        , "   " ++ moduleTargetString t0
        , "   " ++ moduleTargetString t1
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


