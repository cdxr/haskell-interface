### modules
`Data.Interface.Module`

   - This module defines `ModuleInterface` and associated types, most of which are placeholders.
   - Several types are implemented as wrappers around `String`s; the challenge in this module is to decide the appropriate
     representation of those types.
   - This module does not depend on GHC.

`LoadModuleInterface`

   - This is a temporary module that uses GHC to construct ModuleInterfaces from compiled modules.
   - It performs a few unsafe operations, but is structured with future development in mind.

`test/print-test-module/Main.hs`

  - This program uses the above modules to produce a `ModuleInterface` for the source file at `test/modules/Test.hs.`
  - You can run it with `cabal run`.


### some quick notes

- I am not fully satisfied with the names I have used for the package, modules, and types.
- As it stands now, comparing two values of type `ModuleInterface` could tell you which definitions were added or removed,
  but not much else
- Keeping GHC implementation details out of `Data.Interface.Module` has many benefits. I see a few possible approaches to this:
    1. Duplicate many of the types defined in GHC, i.e. define representations for values, types, pattern synonyms, etc.
    2. Create an internal module with wrappers around many GHC types. This would serve as a compatibility layer, encapsulating
       changes between GHC versions.
    3. Define `ModuleInterface` with a type parameter that carries the GHC implementation details. This would enable users
       to work in terms of "pure" interfaces by keeping the type parameter free or constrained by a typeclass. When comparing two
       interfaces, the type parameter would be resolved and GHC types could be compared directly.
