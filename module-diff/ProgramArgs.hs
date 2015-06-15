module ProgramArgs where

import System.FilePath ( (</>) )


data ProgramArgs = ProgramArgs
    { programTarget  :: Target
    , dumpInterfaces :: Bool
    }

defaultArgs :: ProgramArgs
defaultArgs = ProgramArgs
    { programTarget = snd . head $ availableTargets
    , dumpInterfaces = False
    }
  where
    moduleDir = "test" </> "modules"


data Target = Target FilePath FilePath
    deriving (Show, Eq, Ord)

-- | Targets that are built into the program
availableTargets :: [(String, Target)]
availableTargets =
    [ makeTarget "test" "original" ("Test.hs", "TestChangeAll.hs")
    , makeTarget "tagged" "tagged" $
        let path = "Data" </> "Tagged.hs"
        in ("tagged-0.6.1" </> path, "tagged-0.8.0.1" </> path)
    ]
  where
    makeTarget name dir (a, b) =
        let dir' = "test" </> "modules" </> dir
        in (name, Target (dir' </> a) (dir' </> b))


parseArgs :: [String] -> ProgramArgs
parseArgs args0 = go args0 defaultArgs
  where
    go args pa0 = case args of
        [] -> pa0
        "--dump":as ->
            go as $ pa0 { dumpInterfaces = True }
        "--target" : targetName : as
            | Just t <- lookup targetName availableTargets ->
                go as $ pa0 { programTarget = t }
            | otherwise ->
                error $ "not a built-in target: " ++ targetName
        [target0, target1] ->
            pa0 { programTarget = Target target0 target1 }
        _ -> error $ "bad program args: " ++ unwords args
