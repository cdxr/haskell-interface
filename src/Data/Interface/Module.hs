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
  , DeclInfo(..)
  , Type
  , Kind
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

{- ModuleInterface notes:
     - `moduleDecl` will be broken into several fields, distinguished
            by namespace (TODO)
        Note: type constructors and data constructors sharing a name
              currently erase each other from the map
-}


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


type Type = String      -- ^ TODO
type Kind = [String]    -- ^ TODO

-- | A top-level declaration
data Decl = Decl !DeclName DeclInfo
    deriving (Show, Eq, Ord)

declName :: Decl -> DeclName
declName (Decl n _) = n


-- | The content of a top-level declaration, without an identifier
data DeclInfo
    = Value String           -- ^ top-level value/identifier
    | PatternSyn Type        -- ^ a pattern synonym
    | DataCon Type           -- ^ data constructor
    | DataType Kind          -- ^ a newtype/data declaration
    | TypeSyn Kind String    -- ^ a type synonym w/ a kind and definition
    | TypeClass Kind         -- ^ a typeclass
    deriving (Show, Eq, Ord)

{- DeclInfo notes:
      - Each of these constructors will later become distinct types
      - TypeSyn contains its definition, because this affects its interface
            (this is only a String for now, but will have to include
             first-class type information)
      - Type constructors will need a list of their data constructors
      - The distinction between data constructors and other values might
        be problematic

      TODO:
        - type/data families
-}


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
    = LocalExport Decl
    | ReExport QualName
    deriving (Show, Eq, Ord)


-- | All module exports
moduleExports :: ModuleInterface -> [Export]
moduleExports modIf = map LocalExport (Map.elems $ moduleDecls modIf)
                   ++ map ReExport (Set.toList $ moduleReexports modIf)
