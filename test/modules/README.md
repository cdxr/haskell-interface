### test/modules

These directories contain modules used for testing the library.

`original`
 - Contains original modules defined specifically for testing
 - `Test.hs` aims to provide a module containing every possible interface
   element within the scope of this library.

`tagged`
 - Two fairly recent versions of `Data.Proxy` and `Data.Tagged` from
   the `tagged` package
 - `Data.Tagged` chosen for the following properties:
    - Haskell 98
    - relatively small
    - no dependencies except base
    - frequent use of type variables
    - defines many class instances
