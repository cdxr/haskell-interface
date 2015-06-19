{-| This is a temporary module that uses GHC to construct `ModuleInterface`
    values. All interaction with GHC is currently encapsulated in this module.
 -}

module LoadModuleInterface where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import qualified Data.Set as Set

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

import Data.Interface as Interface

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

    withFreshTypeEnv $
        forM (listModSummaries moduleGraph) $
            makeModuleInterface <=< lift . (typecheckModule <=< parseModule)
  where
    -- Sort the modules topologically and discard hs-boot modules.
    -- The topological order is not currently used, but it will be
    -- necessary in the future
    listModSummaries :: ModuleGraph -> [ModSummary]
    listModSummaries = filter (not . isBootSummary) . sortGraph

    sortGraph :: ModuleGraph -> ModuleGraph
    sortGraph g = flattenSCCs $ topSortModuleGraph False g Nothing

    withFreshTypeEnv :: (Monad m) => StateT TypeEnv m a -> m a
    withFreshTypeEnv m = evalStateT m emptyTypeEnv


makeModuleInterface ::
    (GhcMonad m) =>
    GHC.TypecheckedModule -> StateT TypeEnv m ModuleInterface
makeModuleInterface tcMod = do
    _ <- lift $ loadModule tcMod

    let modInfo = GHC.moduleInfo tcMod
        thisMod = ms_mod . pm_mod_summary $ tm_parsed_module tcMod
        modName = makeModuleName thisMod

    (exportList, valueDecls, typeDecls) <-
        splitExports <$> mapM (loadExport modName) (modInfoExports modInfo)

    instances <- mapM loadClassInstance $ modInfoInstances modInfo

    typeMap <- gets $ lookupModuleTypeMap modName

    pure ModuleInterface
        { Interface.moduleName = modName
        , moduleTypes = typeMap
        , moduleValueDecls = nameMapFromList valueDecls
        , moduleTypeDecls  = nameMapFromList typeDecls
        , moduleExportList = exportList
        , moduleInstances  = Set.fromList instances
        }
  where
    loadClassInstance ::
        (Monad m) =>
        GHC.ClsInst ->
        StateT TypeEnv m ClassInstance
    loadClassInstance ci = pure $  --TODO  load associated types into map
        ClassInstance (getOccString $ InstEnv.is_cls_nm ci)  -- class name
                      (map makeType $ InstEnv.is_tys ci)     -- instance types


loadExport ::
    (GhcMonad m) =>
    Interface.ModuleName -> GHC.Name -> StateT TypeEnv m Export
loadExport thisModule ghcName = do
    mTyThing <- lift $ GHC.lookupName ghcName
    case mTyThing of
        Nothing ->
            error $ "loadExport: failed to find name " ++ showQualName q
        Just thing
            | qualModuleName q /= thisModule ->
                pure . ReExport $ SomeName (tyThingNamespace thing) <$> q
            | otherwise ->
                makeLocalExport thing
  where
    q :: Qual RawName
    q = makeQualName ghcName


-- | TODO LOAD TYPE INFORMATION
makeLocalExport :: (GhcMonad m) => GHC.TyThing -> StateT TypeEnv m Export
makeLocalExport thing = case thing of
    ACoAxiom{} -> error "makeLocalExport: ACoAxiom unimplemented"
    AnId a ->                                       -- value
        pure $ mkValueDecl $ Value $ makeType (idType a)
    AConLike (ConLike.RealDataCon dcon) ->          -- data constructor
        pure $ mkValueDecl $ makeDataCon dcon
            -- DataCon (makeType $ dataConType dcon) (makeDataConList dcon)
    AConLike (ConLike.PatSynCon patsyn) ->          -- pattern synonym
        pure $ mkValueDecl $ PatternSyn $ makeType $ PatSyn.patSynType patsyn
    ATyCon tyCon
        | Just rhs <- synTyConRhs_maybe tyCon ->    -- type synonyms
            pure $ mkTypeDecl $ TypeSyn kind $ unsafeOutput rhs
        | isClassTyCon tyCon ->                     -- class definitions
            pure $ mkTypeDecl $ TypeClass kind
        | otherwise ->                              -- data/newtype/other
            pure $ mkTypeDecl $ Interface.DataType kind (makeDataConList tyCon)
      where
        kind = makeKind $ TyCon.tyConKind tyCon
  where
    mkValueDecl :: ValueDecl -> Export
    mkValueDecl = LocalValue . makeQualNamed thing

    mkTypeDecl :: TypeDecl -> Export
    mkTypeDecl = LocalType . makeQualNamed thing


makeDataCon :: GHC.DataCon -> ValueDecl
makeDataCon dcon = DataCon (makeType ghcType) fields
  where
    ghcType = dataConType dcon
    fields = map mkField (dataConFieldLabels dcon)
    mkField lbl = makeNamed lbl ()


-- TODO: type family instances
makeTypeCon :: GHC.TyCon -> Interface.TypeCon
makeTypeCon tc
    | Just _cls <- TyCon.tyConClass_maybe tc =
        typeCon ConClass
        --Dict [makeClass cls]
    | TyCon.isAlgTyCon tc   =
        typeCon ConAlgebraic
    | TyCon.isTypeSynonymTyCon tc =
        typeCon ConSynonym
    | TyCon.isFamInstTyCon tc =
        error "makeTypeCon: family instances not implemented"
    | otherwise =
        error $ "makeTypeCon: unrecognized: " ++ unsafeOutput tc
--  | isFunTyCon tc = 
--  | isPrimTyCon tc = 
--  | isTupleTyCon tc = 
--  | isUnboxedTupleTyCon tc = 
--  | isBoxedTupleTyCon tc = 
--  | isPromotedDataCon tc = 
  where
    typeCon info = TypeCon info kind
    kind = makeKind $ TyCon.tyConKind tc


makeDataConList :: GHC.TyCon -> DataConList
makeDataConList tc
    | TyCon.isAbstractTyCon tc = Abstract
    | otherwise = DataConList $ map mkDataCon (tyConDataCons tc)
  where
    mkDataCon :: GHC.DataCon -> Named ()
    mkDataCon dc = makeNamed dc ()


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
            FunKind (makeKind ka) (makeKind kb)
        | Kind.isLiftedTypeKind k   -> StarKind
        | Kind.isUnliftedTypeKind k -> HashKind
        | Kind.isConstraintKind k   -> ConstraintKind
        | Kind.isSuperKind k        -> SuperKind
        | otherwise                 -> KindVar $ unsafeOutput k


makeRawName :: (GHC.NamedThing n) => n -> RawName
makeRawName = getOccString . GHC.getName


tyThingNamespace :: GHC.TyThing -> Interface.Namespace
tyThingNamespace tyThing = case tyThing of
    AnId{}     -> Values
    AConLike{} -> Values
    ATyCon{}   -> Types
    ACoAxiom{} -> Types


makeNamed :: (GHC.NamedThing n) => n -> a -> Named a
makeNamed n = Named (makeRawName ghcName) (makeOrigin ghcName)
  where
    ghcName = GHC.getName n


ghcNameModule :: GHC.Name -> Interface.ModuleName
ghcNameModule = makeModuleName . GHC.nameModule


makeQualName :: (GHC.NamedThing n) => n -> Qual RawName
makeQualName n = Interface.Qual (ghcNameModule ghcName) (makeRawName ghcName)
  where
    ghcName = GHC.getName n


makeQualNamed :: (GHC.NamedThing n) => n -> a -> Qual (Named a)
makeQualNamed n = Interface.Qual (ghcNameModule ghcName) . makeNamed ghcName
  where
    ghcName = GHC.getName n


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
