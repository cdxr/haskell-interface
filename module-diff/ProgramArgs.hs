module ProgramArgs
(
    parseProgramArgs
  , ProgramArgs(..)
  , Flag
  , OutputFormat(..)
)
where

import Data.Foldable
import Options.Applicative

import LoadPackageInterface

import Task


parseProgramArgs :: IO ProgramArgs
parseProgramArgs =
    execParser $ info (helper <*> mainParser) $ mconcat
        [ fullDesc
        , header "module-diff: view and compare module interfaces"
        ]

type Flag = Bool

data ProgramArgs = ProgramArgs
    { hideString :: Maybe String        -- internal
    , outputClassInstances :: Flag      -- internal
    , outputFormat :: OutputFormat
    , outputFile :: Maybe FilePath
    , programTask :: TargetTask
    } deriving (Show, Eq, Ord)


data OutputFormat
    = OutputConsole
    | OutputHtml
    deriving (Show, Eq, Ord)


mainParser :: Parser ProgramArgs
mainParser =
  ProgramArgs
    <$> optional opt_hide
    <*> flag_instances
    <*> parseFormat
    <*> parseFile
    <*> parseTask

opt_hide :: Parser String
opt_hide = option str $ mconcat
    [ long "hide"
    , help "Hide interface elements referencing name"
    , metavar "NAME"
    , internal
    ]

flag_instances :: Parser Flag
flag_instances = switch $ mconcat
    [ long "instances"
    , help "Include class instances"
    , internal
    ]

parseTask :: Parser TargetTask
parseTask = subparser $ mconcat
    [ command "show-package" $
        info (helper <*> parsePrintPackageInterface) $
            progDesc "Print a package interface summary"
    , command "show" $
        info (helper <*> parsePrintModule) $
            progDesc "Print a module interface summary"
    {-
    , command "compare-packages" $
        info (helper <*> parseComparePackageInterfaces) $
            progDesc "Compare two package interfaces"
    -}
    , command "compare" $
        info (helper <*> parseCompareModules) $
            progDesc "Compare two module interfaces"
    {-
    , command "test" $
        info (helper <*> parseRunTestModule) $
            progDesc "Run a \"test\" module"
    -}
    ]

parsePackageTarget :: Parser PackageTarget
parsePackageTarget =
    PackageTarget
        <$> argument packageFilter (metavar "PACKAGE-TARGET")
        <*> asum [ mkStack . (:[]) <$> argument pkgDB (metavar "PACKAGE-DB")
                 , pure $ mkStack []
                 ]
  where
    mkStack dbs = [GlobalPackageDB, UserPackageDB] ++ dbs

pkgDB :: ReadM PackageDB
pkgDB = SpecificPackageDB <$> str

packageFilter :: ReadM PackageFilter
packageFilter = readPackageFilter <$> str

parsePrintPackageInterface :: Parser (Task PackageTarget m)
parsePrintPackageInterface = PrintPackage <$> parsePackageTarget

parsePrintModule :: Parser (Task p ModuleTarget)
parsePrintModule = PrintModule <$> argument str (metavar "MODULE")

parseFormat :: Parser OutputFormat
parseFormat = flag OutputConsole OutputHtml $ mconcat
    [ long "html"
    , help "Output as HTML"
    ]

parseFile :: Parser (Maybe FilePath)
parseFile = option (Just <$> str) $ mconcat
    [ short 'o'
    , help "Output file"
    , metavar "FILE"
    , value Nothing
    ]


{-
parseComparePackageInterfaces :: Parser Task
parseComparePackageInterfaces =
    ComparePackages
        <$> argument str (metavar "OLD-PACKAGE")
        <*> argument str (metavar "NEW-PACKAGE")
-}

parseCompareModules :: Parser (Task p ModuleTarget)
parseCompareModules =
    CompareModules
        <$> argument str (metavar "OLD-MODULE")
        <*> argument str (metavar "NEW-MODULE")

{-
parseRunTestModule :: Parser Task
parseRunTestModule = RunTestModule <$> option str fields
  where
    fields = long "test"
          <> short 't'
          <> metavar "TEST-TARGET"
-}
