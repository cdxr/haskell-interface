module ProgramArgs where

import System.FilePath ( (</>) )


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


parseArgs :: [String] -> ProgramArgs
parseArgs = foldr f defaultArgs
  where
    f a args = case a of
        "--dump" -> args { dumpInterfaces = True }
        _        -> error $ "bad program argument: " ++ a
