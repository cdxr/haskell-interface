{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import LoadModuleInterface
import LoadPackageInterface

import Data.Interface
import Data.Interface.Change

import ProgramArgs
import Builtin ( builtinTasks )
import Render ( Render(..), renderStdout )


main :: IO ()
main = runMain start =<< parseProgramArgs


start :: Main ()
start = do
    whenFlag_ outputClassInstances $
        error "--instances: unimplemented"

    runTask =<< getArg programTask


runTask :: Task -> Main ()
runTask task = case task of
    PrintPackage t ->
        printPackageInterface =<< loadPackage t
    PrintModule t ->
        printModuleInterface =<< loadModule t
    CompareModules t0 t1 -> do
        mdiff <- diff <$> loadModule t0 <*> loadModule t1
        printModuleDiff (t0, t1) mdiff
    BuiltInTask s -> case lookup s builtinTasks of
        Nothing -> error $ "not a built-in task: " ++ s
        Just t -> runTask t
    RunTestModule t -> runTestModule t


loadPackage :: PackageTarget -> Main PackageInterface
loadPackage (PackageTarget pkgFilter dbs) = do
    liftIO $ print pkgFilter
    lps <- withPackageEnv $ \env -> packageLocations env pkgFilter dbs
    case lps of
        [] -> error $ "could not find package: " ++ showPackageFilter pkgFilter
        lp:_ -> liftIO $ makePackageInterface lp


loadModule :: ModuleTarget -> Main ModuleInterface
loadModule t = do
    iface <- liftIO $ withGhc $ makeInterface =<< guessGoal t

    -- don't qualify names from this module
    unqualify $ moduleName iface  

    ms <- getArg hideString
    pure $ case ms of
        Nothing -> iface
        Just n -> filterInterfaceNames (/= n) iface


printPackageInterface :: PackageInterface -> Main ()
printPackageInterface iface = do
    outputLine $ unlines
        [ "\n*** Package: ***\n"
        , show (pkgId iface)
        , "\nExposed modules:"
        ]

    mapM_ render (pkgExposedModules iface)

printModuleInterface :: ModuleInterface -> Main ()
printModuleInterface iface = do
    outputLine $ unlines
        [ "\n*** Module: " ++ moduleName iface ++ " ***\n"
        , "Exposed type constructors:\n"
        ]

    mapM_ render (moduleTypeCons iface)

    outputLine "Module exports:\n"

    mapM_ render (compileModuleExports iface)


printModuleDiff :: (ModuleTarget, ModuleTarget) -> ModuleDiff -> Main ()
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


-- | This function is horribly inefficient, but is only used for processing
-- "test modules". A test module is a module containing exports with names
-- suffixed with "_0" and "_1", which are compared as if they originated in
-- different module versions.
runTestModule :: ModuleTarget -> Main ()
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


type Env = (ProgramArgs, PackageEnv)

newtype Main a = Main (ReaderT Env (StateT QualContext IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

runMain :: Main a -> ProgramArgs -> IO a
runMain (Main m) args = do
    e <- newPackageEnv
    evalStateT (runReaderT m (args, e)) defQualContext

getArg :: (ProgramArgs -> a) -> Main a
getArg f = Main $ asks (f . fst)

withPackageEnv :: (PackageEnv -> IO a) -> Main a
withPackageEnv f = liftIO . f =<< Main (asks snd)

getQualContext :: Main QualContext
getQualContext = Main (lift get)

unqualify :: ModuleName -> Main ()
unqualify = Main . lift . modify . unqualifyModule

whenFlag :: (ProgramArgs -> Flag) -> Main a -> Main (Maybe a)
whenFlag f m = do
    b <- getArg f
    if b then Just <$> m else pure Nothing

whenFlag_ :: (ProgramArgs -> Flag) -> Main () -> Main ()
whenFlag_ f m = do
    b <- getArg f
    when b m


outputLine :: String -> Main ()
outputLine = liftIO . putStrLn


render :: (Render a) => a -> Main ()
render a = do
    qc <- getQualContext
    liftIO $ renderStdout 2 qc a
    outputLine ""
