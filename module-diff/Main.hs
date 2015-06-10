{-# Language GADTs #-}

module Main where

import Control.Monad
import System.FilePath ( (</>) )
import qualified System.Environment

import qualified System.Console.ANSI as ANSI

import LoadModuleInterface   ( readModuleInterfaces )
import Data.Interface.Module
import Data.Interface.ModuleDiff
import Data.Interface.Change


main :: IO ()
main = do
    args <- parseArgs <$> System.Environment.getArgs
    mdiff <- prepareModuleDiff args
    reportChanges mdiff


prepareModuleDiff :: ProgramArgs -> IO ModuleDiff
prepareModuleDiff args =
    moduleDiff <$> loadModule target0 <*> loadModule target1
  where
    (target0, target1) = targetModules args

    loadModule :: String -> IO ModuleInterface
    loadModule target = do
        [modif] <- readModuleInterfaces [target]
        when (dumpInterfaces args) $
            dumpModuleInterface modif
        pure modif
        
    
dumpModuleInterface :: ModuleInterface -> IO ()
dumpModuleInterface modIf = do
    putStrLn $ "Printing exports for: " ++ moduleName modIf
    forM_ (moduleExports modIf) $
        putStrIndent 2 . show
  where
    putStrIndent i s = putStrLn $ replicate i ' ' ++ s


reportChanges :: ModuleDiff -> IO ()
reportChanges mdiff = do
    forM_ (elemChanges mdiff) $ putStrLn . ansiModuleChange
    ANSI.setSGR [ANSI.Reset]


ansiModuleChange :: AChange ModuleElem -> String
ansiModuleChange (AChange t c) = ansiChangeWith (showModuleElem t) c
  where
    showModuleElem :: ModuleElem a -> a -> String
    showModuleElem tag = case tag of
        Name'{}     -> showString "Module Name: " . show
        Decl'{}     -> show
        Reexport'{} -> showString "Re-export: " . show
        Instance'{} -> show


ansiChangeWith :: (a -> String) -> Change a -> String
ansiChangeWith show' c = case c of
    Removed a  -> setColor ANSI.Red ++ "-  " ++ show' a
    Added a    -> setColor ANSI.Green ++ "+  " ++ show' a
    Change a b ->
        setColor ANSI.Blue ++ "   " ++ show' a ++ "\n => " ++ show' b
  where
    setColor :: ANSI.Color -> String
    setColor c = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull c]


data ProgramArgs = ProgramArgs
    { targetModules  :: (FilePath, FilePath)
    , dumpInterfaces :: Bool
    }

defaultArgs :: ProgramArgs
defaultArgs = ProgramArgs
    { targetModules =
        (moduleDir </> "Test.hs", moduleDir </> "TestChangeAll.hs")
    , dumpInterfaces = False
    }
  where
    moduleDir = "test" </> "modules"


-- TODO implement this for real
parseArgs :: [String] -> ProgramArgs
parseArgs = foldr f defaultArgs
  where
    f a args = case a of
        "--dump" -> args { dumpInterfaces = True }
        _        -> error $ "bad program argument: " ++ a
