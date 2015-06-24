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

    runTask =<< getArg programTask


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
    RunTestModule t -> runTestModule t


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


-- | This function is horribly inefficient, but is only used for processing
-- "test modules". A test module is a module containing exports with names
-- suffixed with "_0" and "_1", which are compared as if they originated in
-- different module versions.
runTestModule :: Target -> Main ()
runTestModule t = do
    iface <- loadModule t

    let iface0 = refineInterface "_0" (not . belong1) iface
        iface1 = refineInterface "_1" (not . belong0) iface

    printModuleInterface iface0
    printModuleInterface iface1

    printModuleDiff (t ++ "_0", t ++ "_1") $
        diffModules iface0 iface1

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
    b <- getArg f
    if b then Just <$> m else pure Nothing

whenFlag_ :: (ProgramArgs -> Flag) -> Main () -> Main ()
whenFlag_ f m = do
    b <- getArg f
    when b m


outputLine :: String -> Main ()
outputLine = Main . lift . putStrLn


renderTree :: RenderTree -> Main ()
renderTree rt = Main . lift $
    renderStdout (indent 2) rt >> putStrLn ""

