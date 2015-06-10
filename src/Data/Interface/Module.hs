{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Interface.Module
 (
-- * ModuleInterface
-- ** Types
    ModuleInterface
-- ** Construction
  , emptyModuleInterface
  , makeModuleInterface
-- ** Components
  , ClassInstance(..)
  , Export(..)
-- ** Inspection
  , moduleName
  , moduleExports
  , moduleValues
  , moduleTypes
  , moduleReexports
  , moduleInstances
-- * Re-exports
  , module Data.Interface.Name
  , module Data.Interface.Module.Decl
 )
where


import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Interface.Name

import Data.Interface.Module.Decl


-- | A module's visible interface, consisting of all components that are
-- exposed to dependent packages. ModuleInterface is the basis for comparison
-- when examining differences between module versions.
data ModuleInterface = ModuleInterface
    { moduleName       :: !ModuleName
    -- locally-defined:
    , moduleValues     :: !(Map ValueName ValueDecl)
    , moduleTypes      :: !(Map TypeName TypeDecl)
    -- re-exported:
    , moduleReexports  :: !(Set (Qual SomeName))
    -- implicitly exported:
    , moduleInstances  :: !(Set ClassInstance)
    }

deriving instance Show ModuleInterface


{- ModuleInterface notes:

   - This will only contain info that is visible to dependent packages.
   - Re-exports are currently sets of names, but later they will map to
     other ModuleInterfaces
   - Class instances are only stubs now; type/data family instances are not
     considered yet
-}


emptyModuleInterface :: ModuleName -> ModuleInterface
emptyModuleInterface name = ModuleInterface
    { moduleName      = name
    , moduleValues    = Map.empty
    , moduleTypes     = Map.empty
    , moduleReexports = Set.empty
    , moduleInstances = Set.empty
    }

makeModuleInterface
    :: ModuleName -> [Export] -> [ClassInstance] -> ModuleInterface
makeModuleInterface name exports instances =
    foldr addExport newModIf exports
  where 
    newModIf :: ModuleInterface
    newModIf =
        (emptyModuleInterface name)
            { moduleInstances = Set.fromList instances }


addExport :: Export -> ModuleInterface -> ModuleInterface
addExport e = case e of
    LocalExport decl -> addDecl decl
    Reexport name -> addReexport name


addDecl :: SomeDecl -> ModuleInterface -> ModuleInterface
addDecl sd modInt = case sd of
    ValueDecl decl -> modInt
        {moduleValues = Map.insert (declName decl) decl (moduleValues modInt)}
    TypeDecl decl -> modInt
        {moduleTypes = Map.insert (declName decl) decl (moduleTypes modInt)}


addReexport :: Qual SomeName -> ModuleInterface -> ModuleInterface
addReexport qual modInt = modInt
        {moduleReexports = Set.insert qual (moduleReexports modInt)}


type ClassName = String

-- | A class instance definition, consisting of the class name and list of
-- types to instantiate that class. Each type should match the corresponding
-- kind in the typeclass parameters
data ClassInstance = ClassInstance !ClassName [Type]
    deriving (Show, Eq, Ord)

{- ClassInstance notes:
      - The `ClassName` field will have to be replaced with a value of a
        proper `Class` type, when that is defined.
      - The `[Type]` field might be problematic when used with ConstraintKinds
        (TODO: look into this)
-}



-- | An element of a module's export list
data Export
    = LocalExport SomeDecl
    | Reexport (Qual SomeName)
    deriving (Show)

--deriving instance Show Export

-- | All module exports
moduleExports :: ModuleInterface -> [Export]
moduleExports modIf = concat
    [ map (LocalExport . ValueDecl) $ Map.elems $ moduleValues modIf
    , map (LocalExport . TypeDecl)  $ Map.elems $ moduleTypes modIf
    , map Reexport $ Set.toList $ moduleReexports modIf
    ]
