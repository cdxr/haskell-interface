{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Data.Foldable ( toList )

import qualified System.Console.ANSI as ANSI

import LoadModuleInterface   ( readModuleInterfaces )

import Data.Interface
import Data.Interface.Change

import ProgramArgs
import Format
import Render


main :: IO ()
main = do
    args <- parseProgramArgs
    result <- prepareModuleDiff args
    runReport (reportResult result) args


data Result = Result
    { targetIds     :: (String, String)
    , theModuleDiff :: ModuleDiff
    } deriving (Show)


prepareModuleDiff :: ProgramArgs -> IO Result
prepareModuleDiff args = do
    mdiff <- diffModules <$> loadModule mod0 <*> loadModule mod1
    pure $ Result (mod0, mod1) mdiff
  where
    Target mod0 mod1 = programTarget args

    loadModule :: String -> IO ModuleInterface
    loadModule target = do
        [modif] <- readModuleInterfaces [target]
        when (dumpInterfaces args) $
            dumpModuleInterface modif
        pure modif


dumpModuleInterface :: ModuleInterface -> IO ()
dumpModuleInterface iface = do
    putStrLn $ "\n*** Module: " ++ moduleName iface ++ " ***\n"

    let render a = printRenderTree (indent 2) a >> putStrLn ""

    putStrLn "Exposed type constructors:\n"
    forM_ (moduleTypes iface) $ render . renderTypeCon

    putStrLn "Module exports:\n"
    forM_ (moduleExports iface) $ render . renderExport

{-
    mapM_ (printFormatTree 2)
        [ makeNode "Values:"    $ moduleValueDecls modIf
        , makeNode "Types:"     $ moduleTypeDecls modIf
        , makeNode "Exports:"   $ moduleExportList modIf
        , makeNode "Instances:" $ moduleInstances modIf
        ]
  where
    makeNode :: (Foldable f, Format a) => String -> f a -> FormatTree
    makeNode lbl = formatNode lbl . map format . toList
-}



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


setColor :: ANSI.Color -> Report ()
setColor c = Report . lift $ putStr s
  where
    s = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]

outputLine :: String -> Report ()
outputLine = Report . lift . putStrLn


outputTree :: FormatTree -> Report ()
outputTree = Report . lift . printFormatTree 2


reportResult :: Result -> Report ()
reportResult res = do
    let (t0, t1) = targetIds res
    outputLine $ unlines
        [ ""
        , "************************************"
        , " Comparing Targets:"
        , "   " ++ t0
        , "   " ++ t1
        , "************************************"
        ]
    reportChanges $ theModuleDiff res


reportChanges :: ModuleDiff -> Report ()
reportChanges mdiff = do

    onDiff (diffModuleName mdiff) $ \(Replace a b) ->
        outputLine $ "Module renamed from " ++ show a ++ " to " ++ show b

    reportSummary "Local Values" $ diffMapSummary $ diffModuleValueDecls mdiff
    reportSummary "Local Types"  $ diffMapSummary $ diffModuleTypeDecls mdiff
    reportSummary "Exports"      $ diffSetSummary $ diffModuleExportList mdiff

    whenFlag_ outputClassInstances $
        reportSummary "Instances" $ diffSetSummary $ diffModuleInstances mdiff

  where
    onDiff :: (Applicative m) => Diff c a -> (c -> m b) -> m ()
    onDiff (Same _)  _ = pure ()
    onDiff (Diff c) f = void $ f c


reportSummary :: (Format c, Format a) => String -> DiffSummary c a -> Report ()
reportSummary title summary = do
    outputLine $ unlines
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
        setColor color
        outputLine $ "* " ++ title ++ " (" ++ category ++ ")"
        forM_ es $ outputTree . format
