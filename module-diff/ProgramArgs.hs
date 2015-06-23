module ProgramArgs
(
    parseProgramArgs
  , ProgramArgs(..)
  , Flag
  , Task(..)
  , Target
)
where

import Data.Foldable

import Options.Applicative


parseProgramArgs :: IO ProgramArgs
parseProgramArgs =
    execParser $
        -- info (helper <*> mainParser)
         info (helper <*> mainParser <|> pure defaultProgramArgs)
             (fullDesc <>
                header "module-diff: view and compare module interfaces")

type Flag = Bool

data ProgramArgs = ProgramArgs
    { outputClassInstances :: Flag
    , programTask :: Task
    } deriving (Show, Eq, Ord)

-- | The arguments used when the program is run with no arguments
defaultProgramArgs :: ProgramArgs
defaultProgramArgs = ProgramArgs
    { outputClassInstances = False
    , programTask = BuiltInTask "test"
    }


-- | A path or module name
type Target = String

data Task
    = PrintInterface Target
    | CompareInterfaces Target Target
    | BuiltInTask String
    | RunTestModule FilePath
    deriving (Show, Eq, Ord)


mainParser :: Parser ProgramArgs
mainParser = ProgramArgs
    <$> switch
        ( long "instances"
       <> help "Include class instances" )
    <*> parseTask

parseTask :: Parser Task
parseTask = asum
    [ parseBuiltinTask
    , parseRunTestModule
    , parseCompareInterfaces
    , parsePrintInterface
    ]

parsePrintInterface :: Parser Task
parsePrintInterface = PrintInterface <$> argument str (metavar "TARGET")

parseCompareInterfaces :: Parser Task
parseCompareInterfaces = CompareInterfaces
    <$> argument str (metavar "OLD")
    <*> argument str (metavar "NEW")

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
          <> metavar "ID"
          <> help "Run a built-in target (development feature)"


{-
readBuiltinTarget :: ReadM Target
readBuiltinTarget = str >>= \s -> case lookupBuiltinTarget s of
    Nothing -> readerError $ "not a built-in target: " ++ s
    Just t -> return t
-}

