### modules

`module-diff/Main.hs`

 - You can run this console program with `cabal run`
 - It provides a diff-like comparison of module exports
 - It currently just shows the internal representation of the exports

`Data.Interface.Module`

 - Defines `ModuleInterface` and associated types
 - Does not depend on GHC.

`Data.Interface.ModuleDiff`

 - Defines `ModuleDiff`, a comparison of two `ModuleInterface`s

`Data.Interface.Change`

 - Defines `Change` and `Diff` typse
 - Provides functions for producing `Diffs` for some collection types

`LoadModuleInterface`

 - This is a temporary module that uses GHC to produce ModuleInterfaces
 - It is very rough, and is structured for rapid development
