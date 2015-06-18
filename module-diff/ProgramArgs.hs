module ProgramArgs
(
    parseProgramArgs
  , ProgramArgs(..)
  , Flag
  , Target(..)
)
where

import System.FilePath ( (</>) )

import Options.Applicative


parseProgramArgs :: IO ProgramArgs
parseProgramArgs =
    execParser $
        info (helper <*> mainParser)
             (fullDesc <> header "compare module interfaces")

type Flag = Bool

data ProgramArgs = ProgramArgs
    { dumpInterfaces       :: Flag
    , outputClassInstances :: Flag
    , programTarget        :: Target
    } deriving (Show, Eq, Ord)

data Target = Target FilePath FilePath
    deriving (Show, Eq, Ord)


mainParser :: Parser ProgramArgs
mainParser = ProgramArgs
    <$> switch
        ( long "dump"
       <> short 'd'
       <> help "Print all loaded module interfaces." )
    <*> pure False
    <*> (builtinTarget <|> target)


target :: Parser Target
target = Target
    <$> argument str (metavar "OLD-TARGET")
    <*> argument str (metavar "NEW-TARGET")


readBuiltinTarget :: ReadM Target
readBuiltinTarget = str >>= \s -> case lookupBuiltinTarget s of
    Nothing -> readerError $ "not a built-in target: " ++ s
    Just t -> return t

builtinTarget :: Parser Target
builtinTarget =
    option readBuiltinTarget $
        long "built-in" <>
        short 'b' <>
        metavar "ID" <>
        help "Run a built-in target (development feature)"


lookupBuiltinTarget :: String -> Maybe Target
lookupBuiltinTarget s = lookup s builtinTargets


-- | Targets that are built into the program
builtinTargets :: [(String, Target)]
builtinTargets =
    [ makeTarget "test" "original" ("Test.hs", "TestChangeAll.hs")
    , makeTarget "tagged" "tagged" $
        let path = "Data" </> "Tagged.hs"
        in ("tagged-0.6.1" </> path, "tagged-0.8.0.1" </> path)
    ]
  where
    makeTarget name dir (a, b) =
        let dir' = "test" </> "modules" </> dir
        in (name, Target (dir' </> a) (dir' </> b))
