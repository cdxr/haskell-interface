### modules

`module-diff/Main.hs`
 - You can run this console program with `cabal run`
 - It prints data produced by the library
 - See `test/modules/README.md` for descriptions of the built-in targets

`Data.Interface.Module`
 - Defines `ModuleInterface` and associated types

`Data.Interface.ModuleDiff`
 - Defines `ModuleDiff`, a comparison of `ModuleInterface`s

`Data.Interface.Change`
 - Defines `Change` typeclass and several types
 - Provides functions for comparing containers

`Data.Interface.Name`
 - Provides types and typeclasses for working with names, namespaces, qualifiers, etc.

`Data.Interface.Source`
 - Provides types for working with source locations

`LoadModuleInterface`
 - This is a temporary module that uses GHC to produce ModuleInterfaces
 - It is very rough, and is structured for rapid development
