module Paths where

import Control.Monad
import Data.List 
import System.Directory
import System.FilePath

import ProgramArgs ( Verbosity(..) )
import LoadPackageInterface ( PackageDB(..) )


-- | Determine the default `PackageDB` stack to use when searching for packages.
-- This should be called once at the beginning of the program.
initDefaultDBStack :: Verbosity -> IO [PackageDB]
initDefaultDBStack v = do
    currentDirectory <- getCurrentDirectory
    let sandboxDir = currentDirectory </> ".cabal-sandbox"
    hasLocalSandbox <- doesDirectoryExist sandboxDir
    if (not hasLocalSandbox)
        then defaultStack <$ log "no local sandbox found"
        else do
            sandboxContents <- getDirectoryContents sandboxDir
            case find (isSuffixOf "packages.conf.d") sandboxContents of
                Nothing -> do
                    log "warning: could not find package db in sandbox"
                    pure defaultStack
                Just path -> do
                    let dbPath = sandboxDir </> path
                    log $ "using sandbox package db at " ++ dbPath
                    pure $ defaultStack ++ [SpecificPackageDB dbPath]
  where
    defaultStack = [GlobalPackageDB, UserPackageDB]
    log = when (v >= Verbose) . putStrLn
