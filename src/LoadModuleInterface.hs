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
import qualified PatSyn
import qualified Outputable as Out
import qualified InstEnv
import qualified SrcLoc
import qualified FastString
import qualified Var
import qualified Type
import qualified Kind

import Data.Interface.Source as Module
import Data.Interface.Type as Module
import Data.Interface.Module as Module

import Debug.Trace


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
    modName = makeModuleName thisModule

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
-- This will fail if the Ghc environment cannot find the `Name`.
--
-- Note: this results in qualified names that refer to the original module,
-- rather than the imported module
--  (e.g. Data.List.foldr will appear as Data.Foldable.foldr)
nameToExport :: GHC.Module -> GHC.Name -> Ghc Module.Export
nameToExport thisModule ghcName = do
    Just (thing, _fixity, _, _) <- getInfo False ghcName
    -- TODO: ^ handle this properly: if `name` is not in the GHC
    --         environment, this will trigger an exception

    let sdecl = thingToSomeDecl thing
        nameMod = nameModule ghcName
    
    pure $ if nameMod /= thisModule
        then Reexport $ makeQual nameMod (someDeclName sdecl)
        else LocalExport sdecl


makeQual :: GHC.Module -> a -> Module.Qual a
makeQual = Module.Qual . makeModuleName


-- TODO: type families
thingToSomeDecl :: GHC.TyThing -> Module.SomeDecl
thingToSomeDecl thing = case thing of
    ACoAxiom{} -> error "makeSomeDecl: ACoAxiom unimplemented"
    AnId a ->                                       -- value
        mkValue $ Value $ unsafeOutput (idType a)
    AConLike (ConLike.RealDataCon dcon) ->          -- data constructor
        mkValue $ makeDataCon dcon
    AConLike (ConLike.PatSynCon patsyn) ->          -- pattern synonym
        mkValue $ PatternSyn $ makeType $ PatSyn.patSynType patsyn
    ATyCon tyCon
        | Just rhs <- synTyConRhs_maybe tyCon ->    -- type synonyms
            mkType $ TypeSyn kind $ unsafeOutput rhs
        | isClassTyCon tyCon ->                     -- class definitions
            mkType $ TypeClass kind
        | otherwise ->                              -- data/newtype/other
            mkType $ Module.DataType kind
      where
        kind = makeKind tyCon
                    -- TODO: ^ include "result" kind
  where
    mkValue :: ValueDecl -> SomeDecl
    mkValue = SomeValue . makeNamed (getName thing)

    mkType :: TypeDecl -> SomeDecl
    mkType = SomeType . makeNamed (getName thing)

--  makeDataCon :: GHC.DataCon -> Module.DeclInfo 'Values
    makeDataCon dcon = DataCon $ makeType $ dataConType dcon
      where
        (_tyVars, _thetaType, _types, _resultType) = dataConSig dcon


makeKind :: GHC.TyCon -> Module.Kind
makeKind tyCon = trace t $
    go (map Type.typeKind . Type.mkTyVarTys $ tyConTyVars tyCon)
       (synTyConResKind tyCon)
  where
    go :: [GHC.Kind] -> GHC.Kind -> Module.Kind
    go [] res = mk res
    go (a:args) res = ApplyKind $ mk a :-> go args res

    -- TODO: promoted types
    mk :: GHC.Kind -> Module.Kind
    mk k | Kind.isLiftedTypeKind k   = StarKind
         | Kind.isUnliftedTypeKind k = HashKind
         | Kind.isConstraintKind k = ConstraintKind
         | otherwise =
             error $ "makeKind: unimplemented Kind: " ++ unsafeOutput k
        
    t = unwords
            [ "TRACE makeKind:"
            , getOccString tyCon
            , show $ map (unsafeOutput . Var.varType) $ tyConTyVars tyCon
            , unsafeOutput $ synTyConResKind tyCon
            ]


makeNamed :: GHC.Name -> a -> Named a
makeNamed ghcName = Named (getOccString ghcName) (makeOrigin ghcName)


makeOrigin :: GHC.Name -> Origin
makeOrigin ghcName = case nameSrcSpan ghcName of
    RealSrcSpan ss ->
        let path = FastString.unpackFS $ srcSpanFile ss
            loc0 = SrcLoc.realSrcSpanStart ss
            loc1 = SrcLoc.realSrcSpanEnd ss
        in KnownSource $ Source path $ SrcSpan (mkLoc loc0) (mkLoc loc1)
    us@UnhelpfulSpan{}
        | us == SrcLoc.wiredInSrcSpan -> WiredIn
        | us == SrcLoc.noSrcSpan -> UnknownSource
        | otherwise -> error $ "makeOrigin: " ++ show us
  where
    mkLoc :: GHC.RealSrcLoc -> Module.SrcLoc
    mkLoc loc = Module.SrcLoc (srcLocLine loc) (srcLocCol loc)


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


makeModuleName :: Module -> Module.ModuleName
makeModuleName = moduleNameString . GHC.moduleName


-- | Run a simple GHC session with default session flags
withGhc :: Ghc a -> IO a
withGhc ghcAction =
    runGhc (Just GHC.Paths.libdir) $ do
        -- prepare package state, ignoring output (no need for linking)
        dynFlags <- getSessionDynFlags
        _ <- setSessionDynFlags dynFlags

        defaultCleanupHandler dynFlags ghcAction
