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
import qualified TyCon
import qualified Type
import qualified Kind

import Data.Interface.Source as Interface
import Data.Interface.Type as Interface
import Data.Interface.Module as Interface

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

    thisModule :: GHC.Module
    thisModule = ms_mod . pm_mod_summary . tm_parsed_module $ typMod

    makeClassInstance :: ClsInst -> ClassInstance
    makeClassInstance ci =
        ClassInstance (getOccString $ InstEnv.is_cls_nm ci)  -- class name
                      (map makeType $ InstEnv.is_tys ci)     -- class kind


-- | Produce an `Export` from a `GHC.Interface` and a `GHC.Name` included in
-- that module's export list, distinguishing locally-defined
-- exports from re-exports.
--
-- This will fail if the Ghc environment cannot find the `Name`.
--
-- Note: this results in qualified names that refer to the original module,
-- rather than the imported module
--  (e.g. Data.List.foldr will appear as Data.Foldable.foldr)
nameToExport :: GHC.Module -> GHC.Name -> Ghc Interface.Export
nameToExport thisModule ghcName = do
    Just (thing, _fixity, _, _) <- getInfo False ghcName
    -- TODO: ^ handle this properly: if `name` is not in the GHC
    --         environment, this will trigger an exception

    let sdecl = thingToSomeDecl thing
        nameMod = nameModule ghcName
    
    pure $ if nameMod /= thisModule
        then Reexport $ makeQual nameMod (someDeclName sdecl)
        else LocalExport sdecl


-- TODO: type families
thingToSomeDecl :: GHC.TyThing -> Interface.SomeDecl
thingToSomeDecl thing = case thing of
    ACoAxiom{} -> error "makeSomeDecl: ACoAxiom unimplemented"
    AnId a ->                                       -- value
        mkValueDecl $ Value $ makeType (idType a)
    AConLike (ConLike.RealDataCon dcon) ->          -- data constructor
        mkValueDecl $ DataCon $ makeType $ dataConType dcon
    AConLike (ConLike.PatSynCon patsyn) ->          -- pattern synonym
        mkValueDecl $ PatternSyn $ makeType $ PatSyn.patSynType patsyn
    ATyCon tyCon
        | Just rhs <- synTyConRhs_maybe tyCon ->    -- type synonyms
            mkTypeDecl $ TypeSyn kind $ unsafeOutput rhs
        | isClassTyCon tyCon ->                     -- class definitions
            mkTypeDecl $ TypeClass kind
        | otherwise ->                              -- data/newtype/other
            mkTypeDecl $ Interface.DataType kind
      where
        kind = makeKind $ TyCon.tyConKind tyCon
  where
    mkValueDecl :: ValueDecl -> SomeDecl
    mkValueDecl = SomeValue . makeNamed (getName thing)

    mkTypeDecl :: TypeDecl -> SomeDecl
    mkTypeDecl = SomeType . makeNamed (getName thing)


-- TODO: type family instances
makeTypeCon :: GHC.TyCon -> Interface.TypeCon
makeTypeCon tc
    | Just _cls <- TyCon.tyConClass_maybe tc =
        typeCon ConClass
        --Dict [makeClass cls]
    | TyCon.isAlgTyCon tc   =
        typeCon $ ConAlgebraic dataConList
    | TyCon.isTypeSynonymTyCon tc =
        typeCon ConSynonym
    | TyCon.isFamInstTyCon tc =
        error "makeTyconType: family instances not implemented"
--  | isFunTyCon tc = 
--  | isPrimTyCon tc = 
--  | isTupleTyCon tc = 
--  | isUnboxedTupleTyCon tc = 
--  | isBoxedTupleTyCon tc = 
--  | isPromotedDataCon tc = 
  where
    typeCon info = TypeCon info kind
    kind = makeKind $ TyCon.tyConKind tc

    dataConList :: DataConList
    dataConList | TyCon.isAbstractTyCon tc = Abstract
                | otherwise = DataConList $ map makeDataCon $ tyConDataCons tc

    makeDataCon :: GHC.DataCon -> Named ()
    makeDataCon dc = makeNamed dc ()


-- TODO: numeric and string literals
makeType :: GHC.Type -> Interface.Type
makeType t0 = case splitForAllTys t0 of
    ([], t)
        | Just tyVar <- Type.getTyVar_maybe t ->
            Var $ makeTypeVar tyVar
        | Just (ta, tb) <- Type.splitFunTy_maybe t ->
            Fun (makeType ta) (makeType tb)
        | Just (ta, tb) <- Type.splitAppTy_maybe t ->
            Apply (makeType ta) (makeType tb)
        | Just (tc, ts) <- Type.splitTyConApp_maybe t ->
            applyType (Con $ makeQualNamed tc $ makeTypeCon tc)
                      (map makeType ts)
        | otherwise ->
            error $ "makeType: " ++ unsafeOutput t
    (vs, t) ->
        Forall (map makeTypeVar vs) (makeType t)
  where
    makeTypeVar :: GHC.TyVar -> TypeVar
    makeTypeVar tyVar =
        TypeVar (getOccString tyVar) (makeKind $ Var.tyVarKind tyVar)


-- TODO: promoted types
makeKind :: GHC.Kind -> Interface.Kind
makeKind k0 = case splitForAllTys k0 of
    (_, k)   -- ignore forall, for now
        | Just (ka, kb) <- Type.splitFunTy_maybe k ->
            FunKind $ makeKind ka :-> makeKind kb
        | Kind.isLiftedTypeKind k   -> StarKind
        | Kind.isUnliftedTypeKind k -> HashKind
        | Kind.isConstraintKind k   -> ConstraintKind
        | Kind.isSuperKind k        -> SuperKind
        | otherwise                 -> KindVar $ unsafeOutput k


makeNamed :: (GHC.NamedThing n) => n -> a -> Named a
makeNamed n = Named (getOccString ghcName) (makeOrigin ghcName)
  where
    ghcName = getName n


makeQual :: GHC.Module -> a -> Qual a
makeQual = Interface.Qual . makeModuleName


makeQualNamed :: (GHC.NamedThing n) => n -> a -> Qual (Named a)
makeQualNamed n = makeQual (GHC.nameModule ghcName) . makeNamed ghcName
  where
    ghcName = getName n


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
    mkLoc :: GHC.RealSrcLoc -> Interface.SrcLoc
    mkLoc loc = Interface.SrcLoc (srcLocLine loc) (srcLocCol loc)



-- * Utils

-- | Unsafely show a value of one of GHC's many "Outputable" types.
-- This depends on `unsafeGlobalDynFlags`, which is not guaranteed to be
-- initialized.
--
-- __All uses of this function are temporary placeholders.__
unsafeOutput :: (Out.Outputable a) => a -> String
unsafeOutput = Out.showSDocUnsafe . Out.ppr


makeModuleName :: GHC.Module -> Interface.ModuleName
makeModuleName = moduleNameString . GHC.moduleName


-- | Run a simple GHC session with default session flags
withGhc :: Ghc a -> IO a
withGhc ghcAction =
    runGhc (Just GHC.Paths.libdir) $ do
        -- prepare package state, ignoring output (no need for linking)
        dynFlags <- getSessionDynFlags
        _ <- setSessionDynFlags dynFlags

        defaultCleanupHandler dynFlags ghcAction
