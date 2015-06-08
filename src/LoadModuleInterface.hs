{-| This is a temporary module that uses GHC to construct `ModuleInterface`
    values. All interaction with GHC is currently encapsulated in this module.
 -}

module LoadModuleInterface where

import Control.Monad

import GHC

import qualified GHC.Paths

import Name ( getOccString )
import Digraph ( flattenSCCs )
import HscTypes ( isBootSummary )
import qualified ConLike
import qualified DataCon
import qualified PatSyn
import qualified TyCon
import qualified Outputable as Out
import qualified InstEnv

import Data.Interface.Module as Module


-- | Using a fresh GHC session, produce a `ModuleInterface` for each of
-- the given module targets.
readModuleInterfaces
    :: [String]              -- ^ filepaths and/or module names
    -> IO [ModuleInterface]  -- ^ ModuleInterfaces in topological order
readModuleInterfaces targetStrs =
    withGhc $ do
        -- prepare package state, ignoring output (no need for linking)
        _ <- setSessionDynFlags =<< getSessionDynFlags

        -- infer target modules
        targets <- forM targetStrs $ \s ->
            guessTarget s Nothing

        moduleInterfacesForTargets targets


-- | Produce a `ModuleInterface` for each compilation target in
-- topological order.
moduleInterfacesForTargets
    :: [Target]  -- filepaths and/or module names
    -> Ghc [ModuleInterface]
moduleInterfacesForTargets targets = do
    setTargets targets

    moduleGraph <- depanal [] False

    when (needsTemplateHaskell moduleGraph) $
        error "readModuleInterface: Template Haskell unsupported"  -- TODO

    forM (listModSummaries moduleGraph) $
        typecheckedModuleInterface <=< loadModSummary
  where
    -- Sort the modules topologically and discard hs-boot modules.
    -- The topological order is not currently used, but it will be
    -- necessary in the future
    listModSummaries :: ModuleGraph -> [ModSummary]
    listModSummaries = filter (not . isBootSummary) . sortGraph

    sortGraph :: ModuleGraph -> ModuleGraph
    sortGraph g = flattenSCCs $ topSortModuleGraph False g Nothing

    loadModSummary :: ModSummary -> Ghc TypecheckedModule
    loadModSummary = loadModule <=< typecheckModule <=< parseModule


-- | Produce a `ModuleInterface` from a `GHC.TypecheckedModule`
typecheckedModuleInterface :: TypecheckedModule -> Ghc ModuleInterface
typecheckedModuleInterface typMod = do
    exports <- mapM (nameToExport thisModule) (modInfoExports modInfo)
    let instances = map makeClassInstance (modInfoInstances modInfo)
    pure $ makeModuleInterface modName exports instances
  where
    modInfo = moduleInfo typMod
    modName = showModuleName thisModule

    thisModule :: Module
    thisModule = ms_mod . pm_mod_summary . tm_parsed_module $ typMod

    makeClassInstance :: ClsInst -> ClassInstance
    makeClassInstance ci =
        ClassInstance (getOccString $ InstEnv.is_cls_nm ci)  -- class name
                      (map makeType $ InstEnv.is_tys ci)     -- class kind


-- | Produce an `Export` from a `GHC.Module` and a `GHC.Name` included in
-- that module's export list, distinguishing locally-defined
-- exports from re-exports.
--
-- This function will fail if GHC cannot find the `Name`.
--
-- Note: this results in qualified names that refer to the original module,
-- rather than the imported module
--  (e.g. Data.List.foldr will appear as Data.Foldable.foldr)
nameToExport :: Module -> Name -> Ghc Export
nameToExport thisModule name
    | nameMod /= thisModule =
        pure $ ReExport $ QualName (showModuleName nameMod) nameStr

    | otherwise = do
        Just (tyThing, _fixity, _, _) <- getInfo False name
        -- TODO: ^ handle this properly: if `name` is not in the GHC
        --         environment, this will crash

        pure $ LocalExport $ Decl nameStr $ makeDeclInfo tyThing
  where
    nameMod = nameModule name
    nameStr = getOccString name


-- TODO: type families
makeDeclInfo :: TyThing -> Module.DeclInfo
makeDeclInfo tything = case tything of
    ACoAxiom _ -> error "nameToExport: ACoAxiom unimplemented"
    AnId a ->                                       -- value
        Value $ unsafeOutput (idType a)
    AConLike (ConLike.RealDataCon dcon) ->          -- data constructor
        makeDataCon dcon
    AConLike (ConLike.PatSynCon patsyn) ->          -- pattern synonym
        PatternSyn $ makeType $ PatSyn.patSynType patsyn
    ATyCon tyCon
        | Just rhs <- synTyConRhs_maybe tyCon ->    -- type synonyms
            TypeSyn kind $ unsafeOutput rhs
        | isClassTyCon tyCon ->                     -- class definitions
            TypeClass kind
        | otherwise ->                              -- data/newtype/other
            Module.DataType kind
      where
        kind = map getOccString (tyConTyVars tyCon)
                    -- TODO: ^ include result type
  where
    makeDataCon :: GHC.DataCon -> Module.DeclInfo
    makeDataCon dcon = DataCon $ makeType $ dataConType dcon
      where
        (_tyVars, _thetaType, _types, _resultType) = dataConSig dcon



-- Note: `Type` and `Kind` are currently implemented as lists of Strings

makeType :: GHC.Type -> Module.Type
makeType = unsafeOutput


-- * Utils

-- | Unsafely show a value of one of GHC's many "Outputable" types.
-- This depends on `unsafeGlobalDynFlags`, which is not guaranteed to be
-- initialized.
--
-- __All uses of this function are temporary placeholders.__
unsafeOutput :: (Out.Outputable a) => a -> String
unsafeOutput = Out.showSDocUnsafe . Out.ppr

showModuleName :: Module -> String
showModuleName = moduleNameString . GHC.moduleName


-- | Run a simple GHC session with default session flags
withGhc :: Ghc a -> IO a
withGhc ghcAction =
    runGhc (Just GHC.Paths.libdir) $ do
        -- prepare package state, ignoring output (no need for linking)
        _ <- setSessionDynFlags =<< getSessionDynFlags

        ghcAction
