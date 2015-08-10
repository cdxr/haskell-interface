### haskell-interface

Tools for inspecting and comparing Haskell modules and packages.

**This project is a work in progress.**

#### Example Commands:

Render package interface as single HTML page:

    cabal run -- -o out.html --html show transformers

Render two package interfaces, highlighting their differences:

    cabal run -- -o out.html --html compare transformers-0.4.2.0 transformers-0.4.3.0
