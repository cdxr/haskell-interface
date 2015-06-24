{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Maybe ( mapMaybe )

import qualified System.Console.ANSI as ANSI

import LoadModuleInterface   ( readModuleInterfaces )

import Data.Interface

import ProgramArgs
import Builtin ( builtinTasks )
import Render


main :: IO ()
main = runMain start =<< parseProgramArgs


start :: Main ()
start = do
    whenFlag_ outputClassInstances $
        error "--instances: unimplemented"

    runTask =<< Main (asks programTask)


runTask :: Task -> Main ()
runTask task = case task of
    PrintInterface t ->
        printModuleInterface =<< loadModule t
    CompareInterfaces t0 t1 -> do
        mdiff <- diffModules <$> loadModule t0 <*> loadModule t1
        printModuleDiff (t0, t1) mdiff
    BuiltInTask s -> case lookup s builtinTasks of
        Nothing -> error $ "not a built-in task: " ++ s
        Just t -> runTask t
    RunTestModule _ ->
        error "--test: unimplemented"


loadModule :: Target -> Main ModuleInterface
loadModule t = do
    is <- liftIO $ readModuleInterfaces [t]
    case is of
        [iface] -> do
            ms <- getArg hideString
            pure $ case ms of
                Nothing -> iface
                Just n -> filterInterfaceNames (/= n) iface
        _ -> error "loadModule: failed to load single interface"  -- TODO


printModuleInterface :: ModuleInterface -> Main ()
printModuleInterface iface = liftIO $ do
    putStrLn $ "\n*** Module: " ++ moduleName iface ++ " ***\n"

    let render a = renderStdout (indent 2) a >> putStrLn ""

    putStrLn "Exposed type constructors:\n"
    forM_ (moduleTypeCons iface) $ render . renderTypeCon

    let qc = defQualContext  -- TODO: set `QualContext`

    putStrLn "Module exports:\n"
    forM_ (compileModuleExports iface) $ render . renderExport qc


newtype Main a = Main (ReaderT ProgramArgs IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runMain :: Main a -> ProgramArgs -> IO a
runMain (Main m) args = do
    a <- runReaderT m args
    a <$ ANSI.setSGR [ANSI.Reset]

getArg :: (ProgramArgs -> a) -> Main a
getArg = Main . asks

whenFlag :: (ProgramArgs -> Flag) -> Main a -> Main (Maybe a)
whenFlag f m = do
    b <- Main $ asks f
    if b then Just <$> m else pure Nothing

whenFlag_ :: (ProgramArgs -> Flag) -> Main a -> Main ()
whenFlag_ f = void . whenFlag f


outputLine :: String -> Main ()
outputLine = Main . lift . putStrLn


renderTree :: RenderTree -> Main ()
renderTree rt = Main . lift $
    renderStdout (indent 2) rt >> putStrLn ""


printModuleDiff :: (Target, Target) -> ModuleDiff -> Main ()
printModuleDiff (t0, t1) mdiff = do
    outputLine $ unlines
        [ ""
        , "************************************"
        , " Comparing Targets:"
        , "   " ++ t0
        , "   " ++ t1
        , "************************************"
        ]

    let qc = defQualContext  -- TODO store a `QualifyContext` in the `Result`

    mapM_ renderTree . mapMaybe (renderChangedExportDiff qc) $
        diffModuleExports mdiff
