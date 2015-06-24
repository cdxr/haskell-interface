{-# LANGUAGE RecordWildCards #-}

module Data.Interface.Module
 (
-- * ModuleInterface
    ModuleInterface(..)
  , ExportName
  , ClassInstance(..)
  , emptyModuleInterface
  , lookupOrigin
-- ** Exports
  , Export(..)
  , compileModuleExports
  , splitExports
  , reexports

-- * Re-exports
  , module Data.Interface.Module.Decl
 )
where

import Data.Maybe ( catMaybes, mapMaybe, fromMaybe )

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Interface.Source
import Data.Interface.Name
import Data.Interface.Type

import Data.Interface.Module.Decl


-- | A module's visible interface, consisting of all components that are
-- exposed to dependent packages. ModuleInterface is the basis for comparison
-- when examining differences between module versions.
data ModuleInterface = ModuleInterface
    { moduleName       :: !ModuleName
    , moduleTypeCons   :: !(NameMap TypeCon)
        -- ^ exposed type constructors originating in this module
    , moduleValueDecls :: !(NameMap (Named ValueDecl))
        -- ^ exposed values (identifiers, data constructors, pattern synonyms)
    , moduleTypeDecls  :: !(NameMap (Named TypeDecl))
        -- ^ exposed types (data/newtypes, type synonyms, class definitions)
    , moduleExportList :: ![ExportName]
    , moduleInstances  :: !(Set ClassInstance)

    , moduleOrigins :: Map SomeName Origin
        -- ^ origins of _all_ entities referenced in this interface

 -- , moduleDepends :: !(Set ModuleName) -- cached list of dependencies  TODO
    } deriving (Show)


type ExportName = Qual SomeName

type ClassName = RawName

-- | A class instance definition, consisting of the class name and list of
-- types to instantiate that class. Each type should match the corresponding
-- kind in the typeclass parameters
data ClassInstance = ClassInstance !ClassName [Type]
    deriving (Show, Eq, Ord)

{- ClassInstance notes:
      - The `ClassName` field will have to be replaced with a value of a
        proper `Class` type, when that is defined.
-}

instance TraverseNames ClassInstance where
    traverseNames f (ClassInstance n ts) =
        ClassInstance <$> f n <*> traverse (traverseNames f) ts


emptyModuleInterface :: ModuleName -> ModuleInterface
emptyModuleInterface modName = ModuleInterface
    { moduleName       = modName
    , moduleTypeCons   = emptyNameMap
    , moduleValueDecls = emptyNameMap
    , moduleTypeDecls  = emptyNameMap
    , moduleExportList = []
    , moduleInstances  = Set.empty
    , moduleOrigins    = Map.empty
    }


lookupOrigin :: (HasSomeName n) => n -> ModuleInterface -> Origin
lookupOrigin n =
    fromMaybe UnknownSource . Map.lookup (someName n) . moduleOrigins


-- | module name is not included
instance TraverseNames ModuleInterface where
    traverseNames f ModuleInterface{..} =
        ModuleInterface moduleName
          <$> traverseNames f moduleTypeCons
          <*> traverseNames f moduleValueDecls
          <*> traverseNames f moduleTypeDecls
          <*> traverse (traverseNames f) moduleExportList
          <*> traverseNames f moduleInstances
          <*> traverseNames f moduleOrigins


data Export
    = LocalValue (Named ValueDecl)
    | LocalType (Named TypeDecl)
    | ReExport ExportName
    deriving (Show, Eq, Ord)

exportName :: ModuleName -> Export -> ExportName
exportName modName e = case e of
    LocalValue n -> Qual modName $ someName n
    LocalType n  -> Qual modName $ someName n
    ReExport q   -> q


-- | Produce a tuple containing a list of all export names, a list of all
-- value declarations, and a list of all type declarations.
splitExports ::
    ModuleName ->
    [Export] ->
    ([ExportName], [Named ValueDecl], [Named TypeDecl])
splitExports modName es =
    let (names, mvals, mtypes) = unzip3 $ map makeTup es
    in (names, catMaybes mvals, catMaybes mtypes)
  where
    makeTup e = case e of
        LocalValue a -> (exportName modName e, Just a, Nothing)
        LocalType a  -> (exportName modName e, Nothing, Just a)
        ReExport{}   -> (exportName modName e, Nothing, Nothing)


reexports :: [Export] -> [ExportName]
reexports = mapMaybe reex
  where
    reex (ReExport n) = Just n
    reex _            = Nothing
        

-- | Produce a list all exports provided by the module.
-- This is called `compileModuleExports` rather than "moduleExports" to
-- emphasize the amount of work taking place.
compileModuleExports :: ModuleInterface -> [Export]
compileModuleExports iface = map mkExport (moduleExportList iface)
  where
    mkExport :: ExportName -> Export
    mkExport exName
        | moduleName iface /= modName = ReExport exName
        | otherwise = case namespace exName of
            Values -> LocalValue . get $ moduleValueDecls iface
            Types -> LocalType . get $ moduleTypeDecls iface
      where
        modName = qualModuleName exName

        get :: NameMap a -> a
        get = takeJust . lookupRawName (rawName exName)

        takeJust Nothing =
            error $ "compileModuleExports: missing name: " ++ rawName exName
        takeJust (Just a) = a
            

lookupValueDecl :: ValueName -> ModuleInterface -> Maybe (Named ValueDecl)
lookupValueDecl name = lookupName name . moduleValueDecls
