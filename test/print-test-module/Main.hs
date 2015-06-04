module Main where

import Control.Monad ( forM_ )
import System.FilePath ( (</>) )

import LoadModuleInterface   ( readModuleInterfaces )
import Data.Interface.Module


testModulePath :: FilePath
testModulePath = "test" </> "modules" </> "Test.hs"

putStrIndent :: Int -> String -> IO ()
putStrIndent i s = putStrLn $ replicate i ' ' ++ s

main :: IO ()
main = do
    putStrLn $ "Producing ModuleInterface for module at " ++ testModulePath
    [modIf] <- readModuleInterfaces [testModulePath]

    putStrLn $ "ModuleName: " ++ moduleName modIf

    putStrLn $ "Exports:"
    forM_ (moduleExports modIf) $ \e ->
        putStrIndent 2 $ case e of
            LocalExport (Decl name info) ->
                "Local: " ++ name ++ " (" ++ info ++ ")"
            ReExport qualName ->
                "ReExport: " ++ qualNameString qualName

    putStrLn $ "Class instances:"
    mapM_ (putStrIndent 2 . show) (moduleInstances modIf)
