module ProgramArgs
(
    parseProgramArgs
  , ProgramArgs(..)
  , Flag
  , Task(..)
  , ModuleTarget
  , PackageTarget(..)
)
where

import Data.Foldable
import Options.Applicative

import LoadPackageInterface


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
    , programTask :: Task
    } deriving (Show, Eq, Ord)


data PackageTarget = PackageTarget PackageFilter [PackageDB]
    deriving (Show, Eq, Ord)

-- | A module path or module name
type ModuleTarget = String

data Task
    = PrintPackage PackageTarget
--  | ComparePackages PackageTarget PackageTarget
    | PrintModule ModuleTarget
    | CompareModules ModuleTarget ModuleTarget
--  | RunTestModule FilePath
    deriving (Show, Eq, Ord)


mainParser :: Parser ProgramArgs
mainParser =
  ProgramArgs
    <$> optional opt_hide
    <*> flag_instances
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

parseTask :: Parser Task
parseTask = subparser $ mconcat
    [ command "show-package" $
        info (helper <*> parsePrintPackageInterface) $
            progDesc "Print a package interface summary"
    , command "show" $
        info (helper <*> parsePrintInterface) $
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

parsePrintPackageInterface :: Parser Task
parsePrintPackageInterface = PrintPackage <$> parsePackageTarget

parsePrintInterface :: Parser Task
parsePrintInterface = PrintModule <$> argument str (metavar "MODULE")

{-
parseComparePackageInterfaces :: Parser Task
parseComparePackageInterfaces =
    ComparePackages
        <$> argument str (metavar "OLD-PACKAGE")
        <*> argument str (metavar "NEW-PACKAGE")
-}

parseCompareModules :: Parser Task
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
