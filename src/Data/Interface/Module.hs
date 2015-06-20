module Data.Interface.Module
 (
-- * ModuleInterface
    ModuleInterface(..)
  , ExportName
  , ClassInstance(..)
  , emptyModuleInterface
-- ** Exports
  , Export(..)
  , moduleExports
  , splitExports

-- * Re-exports
  , module Data.Interface.Module.Decl
 )
where

import Data.Maybe ( catMaybes )

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Interface.Name
import Data.Interface.Type

import Data.Interface.Module.Decl


-- | A module's visible interface, consisting of all components that are
-- exposed to dependent packages. ModuleInterface is the basis for comparison
-- when examining differences between module versions.
data ModuleInterface = ModuleInterface
    { moduleName       :: !ModuleName
    , moduleTypes      :: !(Map RawName TypeCon)
    , moduleValueDecls :: !(Map RawName (Named ValueDecl))
    , moduleTypeDecls  :: !(Map RawName (Named TypeDecl))
    , moduleExportList :: ![ExportName]
    , moduleInstances  :: !(Set ClassInstance)
 -- , moduleDepends    :: !(Set ModuleName) -- cached list of dependencies
    } deriving (Show)

type ExportName = Qual SomeName


type ClassName = String

-- | A class instance definition, consisting of the class name and list of
-- types to instantiate that class. Each type should match the corresponding
-- kind in the typeclass parameters
data ClassInstance = ClassInstance !ClassName [Type]
    deriving (Show, Eq, Ord)

{- ClassInstance notes:
      - The `ClassName` field will have to be replaced with a value of a
        proper `Class` type, when that is defined.
-}


emptyModuleInterface :: ModuleName -> ModuleInterface
emptyModuleInterface modName = ModuleInterface
    { moduleName       = modName
    , moduleTypes      = Map.empty
    , moduleValueDecls = Map.empty
    , moduleTypeDecls  = Map.empty
    , moduleExportList = []
    , moduleInstances  = Set.empty
    }


data Export
    = LocalValue (Qual (Named ValueDecl))
    | LocalType (Qual (Named TypeDecl))
    | ReExport ExportName
    deriving (Show, Eq, Ord)

exportName :: Export -> ExportName
exportName e = case e of
    LocalValue q -> fmap someName q
    LocalType q  -> fmap someName q
    ReExport q   -> q


-- | @splitExports es@ is the tuple containing a list of all export names,
-- a list of all value declarations, and a list of all type declarations.
splitExports :: [Export] -> ([ExportName], [Named ValueDecl], [Named TypeDecl])
splitExports es =
    let (names, mvals, mtypes) = unzip3 $ map makeTup es
    in (names, catMaybes mvals, catMaybes mtypes)
  where
    makeTup e = case e of
        LocalValue q -> (exportName e, Just $ unqual q, Nothing)
        LocalType q  -> (exportName e, Nothing, Just $ unqual q)
        ReExport{}   -> (exportName e, Nothing, Nothing)
        

moduleExports :: ModuleInterface -> [Export]
moduleExports iface = map mkExport (moduleExportList iface)
  where
    mkExport :: ExportName -> Export
    mkExport exName
        | moduleName iface /= modName = ReExport exName
        | otherwise = case namespace exName of
            Values -> LocalValue . lookupQual $ moduleValueDecls iface
            Types -> LocalType . lookupQual $ moduleTypeDecls iface
      where
        modName = qualModuleName exName

        lookupQual :: NameMap a -> Qual a
        lookupQual = Qual modName . takeJust . lookupRawName (rawName exName)

        takeJust Nothing =
            error $ "moduleExports: missing name: " ++ rawName exName
        takeJust (Just a) = a
            

lookupValueDecl :: ValueName -> ModuleInterface -> Maybe (Named ValueDecl)
lookupValueDecl name = Map.lookup (rawName name) . moduleValueDecls
