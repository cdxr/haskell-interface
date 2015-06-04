module Data.Interface.Module
 (
-- * ModuleInterface
-- ** Types
    ModuleInterface
-- ** Construction
  , emptyModuleInterface
  , makeModuleInterface
-- ** Components
  , ModuleName
  , Export(..)
  , DeclName
  , QualName(..)
  , qualNameString
  , Decl(..)
  , ClassInstance(..)
-- ** Inspection
  , moduleName
  , moduleExports
  , moduleDecls
  , moduleReexports
  , moduleInstances
 )
where


import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set


type ModuleName = String

-- | The unqualified name of a top-level declaration
type DeclName = String

-- | The fully-qualified name of a top-level declaration
data QualName = QualName !ModuleName !DeclName
    deriving (Show, Read, Eq, Ord)

qualNameString :: QualName -> String
qualNameString (QualName m d) = m ++ '.' : d


-- | A module's visible interface, consisting of all components that are
-- exposed to dependent packages. ModuleInterface is the basis for comparison
-- when examining differences between module versions.
data ModuleInterface = ModuleInterface
    { moduleName      :: !ModuleName
    , moduleDecls     :: !(Map DeclName Decl)  -- ^ Locally-defined exports
    , moduleReexports :: !(Set QualName)       -- ^ Externally-defined exports
    , moduleInstances :: !(Set ClassInstance)  -- ^ All exposed class instances
    } deriving (Show)


emptyModuleInterface :: ModuleName -> ModuleInterface
emptyModuleInterface name = ModuleInterface name Map.empty Set.empty Set.empty

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
addExport (LocalExport decl)  = addDecl decl
addExport (ReExport qualName) = addReexport qualName

addDecl :: Decl -> ModuleInterface -> ModuleInterface
addDecl decl modInt = 
    modInt {moduleDecls = Map.insert (declName decl) decl m0}
  where m0 = moduleDecls modInt

addReexport :: QualName -> ModuleInterface -> ModuleInterface
addReexport qualName modInt = 
    modInt {moduleReexports = Set.insert qualName s0}
  where s0 = moduleReexports modInt


-- | A top-level declaration
data Decl = Decl
    { declName :: !DeclName
    , declInfo :: String    -- ^ TODO
    } deriving (Show, Eq, Ord)


-- | A class instance definition
data ClassInstance = ClassInstance String  -- ^ TODO
    deriving (Show, Eq, Ord)


-- | An element of a module's export list
data Export
    = LocalExport Decl
    | ReExport QualName
    deriving (Show, Eq, Ord)


-- | All module exports
moduleExports :: ModuleInterface -> [Export]
moduleExports modIf = map LocalExport (Map.elems $ moduleDecls modIf)
                   ++ map ReExport (Set.toList $ moduleReexports modIf)
