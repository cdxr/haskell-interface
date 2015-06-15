### test/modules

These directories contain modules used for testing the library.

`original`
 - Contains original modules defined specifically for testing
 - `Test.hs` aims to provide a module containing every possible interface
   element within the scope of this library.
 - `TestChangeAll.hs` seeks to change every definition provided by `Test.hs`

`tagged`
 - Two fairly recent versions of `Data.Tagged` from the `tagged` package
 - `Data.Tagged` chosen for the following properties:
    - widely used, but relatively small
    - Haskell 98 and depends only on `base`
    - frequent use of type variables
    - defines many class instances
