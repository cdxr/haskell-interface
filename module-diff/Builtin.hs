module Builtin where

import System.FilePath ( (</>) )

import ProgramArgs


-- | An association list of Tasks that are built into the executable.
-- This is a temporary development feature.
builtinTasks :: [(String, Task)]
builtinTasks =
    [ makeComparison "test" "original" ("Test.hs", "TestChangeAll.hs")
    , makeComparison "tagged" "tagged" $
        let path = "Data" </> "Tagged.hs"
        in ("tagged-0.6.1" </> path, "tagged-0.8.0.1" </> path)
    ]
  where
    makeComparison name dir (a, b) =
        let dir' = "test" </> "modules" </> dir
        in (name, CompareInterfaces (dir' </> a) (dir' </> b))

