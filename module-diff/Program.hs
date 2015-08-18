{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Program where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import LoadPackageInterface
import Data.Interface

import ProgramArgs
import Paths


data ProgramEnv = ProgramEnv
    { programArgs :: ProgramArgs
    , programDefaultDBs :: [PackageDB]
    , packageEnv :: PackageEnv
    }

newtype Program a = Program (ReaderT ProgramEnv (StateT QualContext IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

runProgram :: Program a -> ProgramArgs -> IO a
runProgram (Program m) args = do
    env <- ProgramEnv args
               <$> Paths.initDefaultDBStack (verbosity args)
               <*> newPackageEnv
    evalStateT (runReaderT m env) defQualContext

getArg :: (ProgramArgs -> a) -> Program a
getArg f = Program $ asks (f . programArgs)

getDefaultDBStack :: Program [PackageDB]
getDefaultDBStack = Program $ asks programDefaultDBs

withPackageEnv :: (PackageEnv -> IO a) -> Program a
withPackageEnv f = liftIO . f =<< Program (asks packageEnv)

getQualContext :: Program QualContext
getQualContext = Program (lift get)

unqualify :: ModuleName -> Program ()
unqualify = Program . lift . modify . unqualifyModule

whenArgs :: (ProgramArgs -> Bool) -> Program a -> Program (Maybe a)
whenArgs f m = do
    b <- getArg f
    if b then Just <$> m else pure Nothing

whenArgs_ :: (ProgramArgs -> Bool) -> Program () -> Program ()
whenArgs_ f m = do
    b <- getArg f
    when b m


verbose :: String -> Program ()
verbose msg = do
    f <- getVerbosePrinter
    liftIO $ f msg

getVerbosePrinter :: Program (String -> IO ())
getVerbosePrinter = getFun <$> getArg verbosity
  where
    getFun :: Verbosity -> String -> IO ()
    getFun Verbose = liftIO . putStrLn
    getFun Quiet = const $ return ()
