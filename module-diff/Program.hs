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

whenFlag :: (ProgramArgs -> Flag) -> Program a -> Program (Maybe a)
whenFlag f m = do
    b <- getArg f
    if b then Just <$> m else pure Nothing

whenFlag_ :: (ProgramArgs -> Flag) -> Program () -> Program ()
whenFlag_ f m = do
    b <- getArg f
    when b m

