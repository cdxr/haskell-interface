module Data.Interface.Module
 (
-- * ModuleInterface
    ModuleInterface(..)
  , ModuleName
  , ExportName
  , ClassInstance(..)
  , makeModuleInterface
  , isLocal
  , findExport
  , unsafeFindExport
  -- , lookupOrigin
  , filterInterfaceNames
-- ** Exports
  , Export
  , ExportElem
  , compileModuleExports
  , splitExports

  , module Data.Interface.Module.Diff
  , module Data.Interface.Module.Entity
 )
where

import Data.Interface.Module.Interface
import Data.Interface.Name ( ModuleName )
import Data.Interface.Module.Export
    ( ExportName, Export, ExportElem, splitExports )

import Data.Interface.Module.Diff
import Data.Interface.Module.Entity
