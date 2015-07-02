{-# LANGUAGE RecordWildCards #-}

module Data.Interface.Module
 (
-- * ModuleInterface
    ModuleInterface(..)
  , ExportName
  , ClassInstance(..)
  , emptyModuleInterface
  , makeModuleInterface
  , isLocal
  , findExport
  , unsafeFindExport
  -- , lookupOrigin
  , filterInterfaceNames
-- ** Exports
  , Export(..)
  , compileModuleExports
  , splitExports

-- * Re-exports
  , module Data.Interface.Module.Decl
 )
where

import Data.Foldable
import Data.Maybe ( catMaybes, fromMaybe )

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Interface.Source
import Data.Interface.Name
import Data.Interface.Name.Map
import Data.Interface.Type
import Data.Interface.Change.OrdSet

import Data.Interface.Module.Decl


-- | A module's visible interface, consisting of all components that are
-- exposed to dependent packages. ModuleInterface is the basis for comparison
-- when examining differences between module versions.
data ModuleInterface = ModuleInterface
    { moduleName       :: ModuleName
    , moduleTypeCons   :: NameMap TypeCon
        -- ^ exposed type constructors originating in this module
    , moduleValueDecls :: NameMap ValueDecl
        -- ^ exposed values (identifiers, data constructors, pattern synonyms)
    , moduleTypeDecls  :: NameMap TypeDecl
        -- ^ exposed types (data/newtypes, type synonyms, class definitions)
    , moduleExportList :: OrdSet ExportName
    , moduleInstances  :: Set ClassInstance

    , moduleOrigins :: NameMap' SomeName Origin
        -- ^ origins of _all_ entities referenced in this interface

 -- , moduleDepends :: !(Set ModuleName) -- cached list of dependencies  TODO
    } deriving (Show)


data ExportName
    = LocalName SomeName
    | ForeignName (Qual SomeName)
    deriving (Show, Eq, Ord)

{- ExportName: We avoid qualifying local exports because when it comes time
   to compare the exports of two modules, it shouldn't matter if the modules
   have the same name. The `Eq` instance will match up local names with local
   names.
-}

instance HasRawName ExportName where
    rawName en = case en of
        LocalName n -> rawName n
        ForeignName q -> rawName q
    rename f en = case en of
        LocalName n -> LocalName $ rename f n
        ForeignName q -> ForeignName $ rename f q

instance TraverseNames ExportName where
    traverseNames f en = case en of
        LocalName sn  -> LocalName <$> traverseNames f sn
        ForeignName q -> ForeignName <$> traverseNames f q



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
    , moduleExportList = mempty
    , moduleInstances  = Set.empty
    , moduleOrigins    = emptyNameMap
    }


-- | Determine if the qualified entity originates in the given module
isLocal :: Qual a -> ModuleInterface -> Bool
isLocal (Qual mn _) iface = moduleName iface == mn


findExport :: ModuleInterface -> ExportName -> Maybe Export
findExport iface en = case en of
    ForeignName q -> Just $ ReExport q
    LocalName n -> case namespace n of
        Values -> LocalValue <$> lookupRawName n (moduleValueDecls iface)
        Types -> LocalType <$> lookupRawName n (moduleTypeDecls iface)


unsafeFindExport :: ModuleInterface -> ExportName -> Export
unsafeFindExport iface en = fromMaybe errMsg (findExport iface en)
  where
    errMsg = error $ "unsafeFindExport: could not find " 
                  ++ show (rawName en) ++ " in module "
                  ++ show (moduleName iface)


lookupOrigin :: (HasSomeName n) => n -> ModuleInterface -> Origin
lookupOrigin n =
    maybe UnknownSource unName . lookupSomeName n . moduleOrigins


hasDecl :: SomeName -> ModuleInterface -> Bool
hasDecl n ModuleInterface{..} = case namespace n of
    Values | Just _ <- lookupRawName (rawName n) moduleValueDecls
        -> True
    Types  | Just _ <- lookupRawName (rawName n) moduleTypeDecls
        -> True
    _ -> False


filterExports :: (ExportName -> Bool) -> ModuleInterface -> ModuleInterface
filterExports f iface = iface
    { moduleExportList = filterOrdSet f $ moduleExportList iface }


-- | Remove every interface entity containing a reference to a name that does
-- not pass the filter.
filterInterfaceNames :: (RawName -> Bool) -> ModuleInterface -> ModuleInterface
filterInterfaceNames f ModuleInterface{..} =
    let iface = ModuleInterface
            { moduleName = moduleName
            , moduleTypeCons   = filterMapNames f moduleTypeCons
            , moduleValueDecls = filterMapNames f moduleValueDecls
            , moduleTypeDecls  = filterMapNames f moduleTypeDecls
            , moduleExportList = filterOrdSet (allNames f) moduleExportList
            , moduleInstances  = Set.filter (allNames f) moduleInstances
            , moduleOrigins    = filterMapNames f moduleOrigins
            }
    in filterExports (keep iface) iface
  where
    keep :: ModuleInterface -> ExportName -> Bool
    keep iface en = case en of
        LocalName n -> hasDecl n iface
        ForeignName{} -> True                   -- keep all re-exports


-- | module name is not included
instance TraverseNames ModuleInterface where
    traverseNames f ModuleInterface{..} =
        ModuleInterface moduleName
          <$> traverseNames f moduleTypeCons
          <*> traverseNames f moduleValueDecls
          <*> traverseNames f moduleTypeDecls
          <*> unsafeTraverseOrdSet (traverseNames f) moduleExportList
          <*> traverseNames f moduleInstances
          <*> traverseNames f moduleOrigins


makeModuleInterface ::
    ModuleName ->
    NameMap TypeCon ->
    [Export] ->
    [ClassInstance] ->
    ModuleInterface
makeModuleInterface modName typeMap exports instances =
    ModuleInterface
        { moduleName       = modName
        , moduleTypeCons   = typeMap
        , moduleValueDecls = makeNameMap valueDecls
        , moduleTypeDecls  = makeNameMap typeDecls
        , moduleExportList = makeOrdSet exportList
        , moduleInstances  = Set.fromList instances
        , moduleOrigins    = emptyNameMap  -- TODO
        }
  where
    (exportList, valueDecls, typeDecls) = splitExports exports


data Export
    = LocalValue (Named ValueDecl)
    | LocalType (Named TypeDecl)
    | ReExport (Qual SomeName)
    deriving (Show, Eq, Ord)

exportName :: Export -> ExportName
exportName e = case e of
    LocalValue n -> LocalName (someName n)
    LocalType n  -> LocalName (someName n)
    ReExport q   -> ForeignName q


-- | Produce a tuple containing a list of all export names, a list of all
-- value declarations, and a list of all type declarations.
splitExports ::
    [Export] ->
    ([ExportName], [Named ValueDecl], [Named TypeDecl])
splitExports es =
    let (names, mvals, mtypes) = unzip3 $ map makeTup es
    in (names, catMaybes mvals, catMaybes mtypes)
  where
    makeTup e = case e of
        LocalValue a -> (exportName e, Just a, Nothing)
        LocalType a  -> (exportName e, Nothing, Just a)
        ReExport{}   -> (exportName e, Nothing, Nothing)


-- | Produce a list all exports provided by the module.
-- This is called `compileModuleExports` rather than "moduleExports" to
-- emphasize the amount of work taking place.
compileModuleExports :: ModuleInterface -> [Export]
compileModuleExports iface =
    map (unsafeFindExport iface) . toList $ moduleExportList iface
            

lookupValueDecl :: ValueName -> ModuleInterface -> Maybe (Named ValueDecl)
lookupValueDecl name = lookupName name . moduleValueDecls
