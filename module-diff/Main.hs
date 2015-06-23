{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Data.Maybe ( mapMaybe )

import qualified System.Console.ANSI as ANSI

import LoadModuleInterface   ( readModuleInterfaces )

import Data.Interface

import ProgramArgs
import Builtin ( builtinTasks )
import Render


main :: IO ()
main = do
    args <- parseProgramArgs

    when (outputClassInstances args) $
        error "--instances: unimplemented"

    runTask args $ programTask args


runTask :: ProgramArgs -> Task -> IO ()
runTask args task = case task of
    PrintInterface t ->
        printModuleInterface =<< loadModule t
    CompareInterfaces t0 t1 -> do
        mdiff <- diffModules <$> loadModule t0 <*> loadModule t1
        printModuleDiff args (t0, t1) mdiff
    BuiltInTask s -> case lookup s builtinTasks of
        Nothing -> error $ "not a built-in task: " ++ s
        Just t -> runTask args t
    RunTestModule _ ->
        error "--test: unimplemented"


loadModule :: Target -> IO ModuleInterface
loadModule t = do
    is <- readModuleInterfaces [t]
    case is of
        [i] -> pure i
        _ -> error "loadModule: failed to load single interface"  -- TODO


printModuleInterface :: ModuleInterface -> IO ()
printModuleInterface iface = do
    putStrLn $ "\n*** Module: " ++ moduleName iface ++ " ***\n"

    let render a = renderStdout (indent 2) a >> putStrLn ""

    putStrLn "Exposed type constructors:\n"
    forM_ (moduleTypeCons iface) $ render . renderTypeCon

    let qc = defQualContext  -- TODO: set `QualContext`

    putStrLn "Module exports:\n"
    forM_ (compileModuleExports iface) $ render . renderExport qc


printModuleDiff :: ProgramArgs -> (Target, Target) -> ModuleDiff -> IO ()
printModuleDiff args ts mdiff =
    runReport (reportModuleDiff ts mdiff) args


newtype Report a = Report (ReaderT ProgramArgs IO a)
    deriving (Functor, Applicative, Monad)

runReport :: Report a -> ProgramArgs -> IO a
runReport (Report m) args = do
    a <- runReaderT m args
    a <$ ANSI.setSGR [ANSI.Reset]

whenFlag :: (ProgramArgs -> Flag) -> Report a -> Report (Maybe a)
whenFlag f m = do
    b <- Report $ asks f
    if b then Just <$> m else pure Nothing

whenFlag_ :: (ProgramArgs -> Flag) -> Report a -> Report ()
whenFlag_ f = void . whenFlag f


outputLine :: String -> Report ()
outputLine = Report . lift . putStrLn


renderTree :: RenderTree -> Report ()
renderTree rt = Report . lift $
    renderStdout (indent 2) rt >> putStrLn ""


reportModuleDiff :: (Target, Target) -> ModuleDiff -> Report ()
reportModuleDiff (t0, t1) mdiff = do
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
