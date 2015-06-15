{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Data.Foldable ( toList )

import qualified System.Environment

import qualified System.Console.ANSI as ANSI

import LoadModuleInterface   ( readModuleInterfaces )
import Data.Interface

import ProgramArgs
import Format


main :: IO ()
main = do
    args <- parseArgs <$> System.Environment.getArgs
    mdiff <- prepareModuleDiff args
    runReport (reportChanges mdiff) args


prepareModuleDiff :: ProgramArgs -> IO ModuleDiff
prepareModuleDiff args =
    diffModules <$> loadModule target0 <*> loadModule target1
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
    putStr $ unlines
        [ ""
        , "*** Dumping ModuleInterface ***"
        , "Name: " ++ moduleName modIf
        ]

    mapM_ (printFormatTree 2)
        [ makeNode "Values:"     $ moduleValues modIf
        , makeNode "Types:"      $ moduleTypes modIf
        , makeNode "Re-exports:" $ moduleReexports modIf
        , makeNode "Instances:"  $ moduleInstances modIf
        ]
  where
    makeNode :: (Foldable f, Format a) => String -> f a -> FormatTree
    makeNode lbl = formatNode lbl . map format . toList


newtype Report a = Report (ReaderT ProgramArgs IO a)
    deriving (Functor, Applicative, Monad)

runReport :: Report a -> ProgramArgs -> IO a
runReport (Report m) args = do
    a <- runReaderT m args
    a <$ ANSI.setSGR [ANSI.Reset]

output :: ANSI.Color -> String -> Report ()
output color s = Report . lift $ putStrLn $ setColor color ++ s
  where
    setColor :: ANSI.Color -> String
    setColor c = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]


outputTree :: FormatTree -> Report ()
outputTree = Report . lift . printFormatTree 2


reportChanges :: ModuleDiff -> Report ()
reportChanges mdiff = do
    onDiff (diffModuleName mdiff) $ \(Replace a b) ->
        output ANSI.White $
            "Module renamed from " ++ show a ++ " to " ++ show b

    reportSummary "Local Values" $ diffMapSummary $ diffModuleValues mdiff
    reportSummary "Local Types"  $ diffMapSummary $ diffModuleTypes mdiff
    reportSummary "Reexports"    $ diffSetSummary $ diffModuleReexports mdiff
    reportSummary "Instances"    $ diffSetSummary $ diffModuleInstances mdiff

  where
    onDiff :: (Applicative m) => Diff c a -> (c -> m b) -> m ()
    onDiff (Same _)  _ = pure ()
    onDiff (Diff c) f = void $ f c


reportSummary :: (Format c, Format a) => String -> DiffSummary c a -> Report ()
reportSummary title summary = do
    output ANSI.White $ unlines
        [ ""
        , "*** " ++ title ++ " ***"
        , show (length $ unchanged summary) ++ " unchanged"
        , show (length $ changed summary) ++ " changed"
        , show (length $ added summary) ++ " added"
        , show (length $ removed summary) ++ " removed"
        ]

    outputAll "changed" ANSI.Blue  $ changed summary
    outputAll "added"   ANSI.Green $ added summary
    outputAll "removed" ANSI.Red   $ removed summary

  where
    outputAll ::
        (Foldable f, Format a) =>
        String -> ANSI.Color -> f a -> Report ()
    outputAll category color es = do
        output color $ "* " ++ title ++ " (" ++ category ++ ")"
        forM_ es $ outputTree . format
