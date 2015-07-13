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

import Data.Interface.Package ( PackageId(..) )


parseProgramArgs :: IO ProgramArgs
parseProgramArgs =
    execParser $
         info (helper <*> mainParser <|> pure defaultProgramArgs) $
            mconcat
                [ fullDesc
                , header "module-diff: view and compare module interfaces"
                ]

type Flag = Bool

data ProgramArgs = ProgramArgs
    { hideString :: Maybe String        -- internal
    , outputClassInstances :: Flag      -- internal
    , programTask :: Task
    } deriving (Show, Eq, Ord)

-- | The arguments used when the program is run with no arguments
defaultProgramArgs :: ProgramArgs
defaultProgramArgs = ProgramArgs
    { hideString = Nothing
    , outputClassInstances = False
    , programTask = BuiltInTask "test"
    }


data PackageTarget = PackageTarget FilePath PackageId
    deriving (Show, Eq, Ord)

-- | A module path or module name
type ModuleTarget = String

data Task
    = PrintPackage PackageTarget
--  | ComparePackages PackageTarget PackageTarget
    | PrintModule ModuleTarget
    | CompareModules ModuleTarget ModuleTarget
    | BuiltInTask String
    | RunTestModule FilePath
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
            progDesc "Print an interface summary"
    {-
    , command "compare-packages" $
        info (helper <*> parseComparePackageInterfaces) $
            progDesc "Compare two package interfaces"
    -}
    , command "compare" $
        info (helper <*> parseCompareInterfaces) $
            progDesc "Compare two module interfaces"
    , command "test" $
        info (helper <*> parseRunTestModule) $
            progDesc "Run a \"test\" module"
    , command "builtin" $
        info (helper <*> parseBuiltinTask) $
            progDesc "Run a built-in task (development feature)"
    ]

parsePackageTarget :: Parser PackageTarget
parsePackageTarget =
    PackageTarget
        <$> argument str (metavar "PACKAGE-DB")
        <*> fmap PackageId (argument str (metavar "PACKAGE-ID"))


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

parseCompareInterfaces :: Parser Task
parseCompareInterfaces =
    CompareModules
        <$> argument str (metavar "OLD-MODULE")
        <*> argument str (metavar "NEW-MODULE")

parseRunTestModule :: Parser Task
parseRunTestModule = RunTestModule <$> option str fields
  where
    fields = long "test"
          <> short 't'
          <> metavar "TEST-TARGET"

parseBuiltinTask :: Parser Task
parseBuiltinTask = BuiltInTask <$> option str fields
  where
    fields = long "built-in"
          <> short 'b'
          <> metavar "TASK-ID"
