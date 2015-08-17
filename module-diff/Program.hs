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


type ProgramEnv = (ProgramArgs, PackageEnv)

newtype Program a = Program (ReaderT ProgramEnv (StateT QualContext IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

runProgram :: Program a -> ProgramArgs -> IO a
runProgram (Program m) args = do
    e <- newPackageEnv
    evalStateT (runReaderT m (args, e)) defQualContext

getArg :: (ProgramArgs -> a) -> Program a
getArg f = Program $ asks (f . fst)

withPackageEnv :: (PackageEnv -> IO a) -> Program a
withPackageEnv f = liftIO . f =<< Program (asks snd)

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
