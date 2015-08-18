{-# LANGUAGE DeriveFunctor #-}

module ProgramArgs
(
    parseProgramArgs
  , ProgramArgs(..)
  , Command(..)
  , Target(..)
  , CompareTarget(..)
  , Verbosity(..)
  , OutputFormat(..)
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

data Command
    = ShowCommand (Target String)
    | CompareCommand (CompareTarget String)
    deriving (Show, Eq, Ord)

data Target a = Target
    { targetDB :: Maybe PackageDB
    , target :: a
    } deriving (Show, Eq, Ord, Functor)

data CompareTarget a
    = CompareThisInstalled
        -- ^ compare working directory package to installed version
    | CompareInstalled (Target a)
        -- ^ compare given package to installed version
    | CompareThese (Target a) (Target a)
        -- ^ compare given packages
    deriving (Show, Eq, Ord, Functor)


data Verbosity = Quiet | Verbose
    deriving (Show, Eq, Ord)

data ProgramArgs = ProgramArgs
    { outputClassInstances :: Bool      -- internal
    , includeNotes :: Bool              -- internal
    , onlyShowChanges :: Bool
    , verbosity :: Verbosity
    , outputFormat :: OutputFormat
    , outputFile :: Maybe FilePath
    , programCommand :: Command
    } deriving (Show, Eq, Ord)


data OutputFormat
    = OutputConsole
    | OutputHtml
    deriving (Show, Eq, Ord)


mainParser :: Parser ProgramArgs
mainParser =
  ProgramArgs
    <$> switch_instances
    <*> switch_notes
    <*> switch_onlyChanges
    <*> flag_verbosity
    <*> parseFormat
    <*> parseFile
    <*> parseCommand


parseCommand :: Parser Command
parseCommand = subparser $ mconcat
    [ command "show" $
        info (helper <*> parseShowCommand) $
            progDesc "Print an interface summary for a package or module"
    , command "compare" $
        info (helper <*> (CompareCommand <$> parseCompare)) $
            progDesc "Compare two packages or modules"
    ]
  where
    parseShowCommand = ShowCommand <$> parseTarget
    parseCompare = asum
        [ CompareThese <$> parseTarget <*> parseTarget
        , CompareInstalled <$> parseTarget
        , pure $ CompareThisInstalled
        ]


parseTarget :: Parser (Target String)
parseTarget = Target <$> optional db <*> argument str (metavar "TARGET")
  where 
    db = option pkgDB $ mconcat
            [ long "package-db"
            , short 'd'
            , metavar "PATH"
            , help "Package DB for next TARGET"
            ]

pkgDB :: ReadM PackageDB
pkgDB = do
    s <- str
    pure $ case s of
        "global" -> GlobalPackageDB
        "user" -> UserPackageDB
        _ -> SpecificPackageDB s
        


switch_instances :: Parser Bool
switch_instances = switch $ mconcat
    [ long "instances"
    , help "Include class instances"
    , internal
    ]

switch_notes :: Parser Bool
switch_notes = switch $ mconcat
    [ long "notes"
    , help "Include hidden notes in the HTML"
    , internal
    ]

switch_onlyChanges :: Parser Bool
switch_onlyChanges = switch $ mconcat
    [ long "only-changes"
    , help "Only output interface changes"
    ]

flag_verbosity :: Parser Verbosity
flag_verbosity = flag Quiet Verbose $ mconcat
    [ long "verbose"
    , short 'v'
    , help "Print diagnostic information"
    ]

packageSelector :: ReadM PackageSelector
packageSelector = readPackageSelector <$> str

parseFormat :: Parser OutputFormat
parseFormat = flag OutputConsole OutputHtml $ mconcat
    [ long "html"
    , help "Output as HTML"
    ]

parseFile :: Parser (Maybe FilePath)
parseFile = optional $ option str $ mconcat
    [ short 'o'
    , help "Output file"
    , metavar "FILE"
    ]
