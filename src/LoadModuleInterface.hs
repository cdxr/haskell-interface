{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LoadModuleInterface
(
    ModuleGoal(..)
  , guessGoal
  , withGhc
  , makeInterface

  , PkgKey
  , pkgKeyToGHC
  , pkgKeyFromCabal
)
where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import qualified Data.Map as Map

import GHC

import qualified GHC.Paths

import DynFlags ( getDynFlags )

import qualified Name
import qualified Module
import Digraph ( flattenSCCs )
import HscTypes ( isBootSummary )
import FastString ( FastString, unpackFS )
import UniqSet
import qualified ConLike
import qualified PatSyn
import qualified Outputable as Out
import qualified InstEnv
import qualified SrcLoc
import qualified Var
import qualified TyCon
import qualified Type
import qualified Kind

import qualified Distribution.Text as Cabal
import qualified Distribution.Package as Cabal

import Data.Interface as Interface
import Data.Interface.Type.Build as Build


-- | A package key stored in the same format as a GHC.PackageKey
newtype PkgKey = PkgKey String
    deriving (Show, Eq, Ord)

pkgKeyToGHC :: PkgKey -> GHC.PackageKey
pkgKeyToGHC (PkgKey s) = Module.stringToPackageKey s

pkgKeyFromCabal :: Cabal.PackageKey -> PkgKey
pkgKeyFromCabal = PkgKey . Cabal.display


-- | The module target; either a source module that needs to be compiled,
-- or the name of a module that is installed or has a local source.
data ModuleGoal
    = SourceGoal GHC.TargetId   -- TODO: change to `Either ModuleName FilePath`
        -- ^ a GHC compilation target
    | ModuleGoal Interface.ModuleName (Maybe PkgKey)
        -- ^ name and optional `PackageKey`; when @Nothing@ the module might
        -- be a compilation target
    deriving (Eq)


-- | Guess the intended `ModuleGoal` from a `String`. When given a path to a source
-- file, it will be compiled to produce the interface. When given the name of
-- a module, the corresponding local source file will be used if available;
-- otherwise, the module will be searched for in the package database.
guessGoal :: String -> Ghc ModuleGoal
guessGoal s = do
    Target tid _ _ <- guessTarget s Nothing
    pure $ case tid of
        TargetModule n -> ModuleGoal (moduleNameString n) Nothing
        _ -> SourceGoal tid

instance Show ModuleGoal where
    showsPrec p g = showParen (p > 10) $ case g of
        SourceGoal t ->
            showString "SourceGoal<" . showTargetId t . showChar '>'
        ModuleGoal modName mPkgKey ->
            showString "ModuleGoal<" .
            shows modName  .
            showChar ' ' .
            shows mPkgKey .
            showChar '>'
      where
        showTargetId t = case t of
            TargetModule n -> showString "TargetModule " . showModName n
            TargetFile fp mp ->
                showString "TargetFile " . shows fp . showChar ' ' . shows mp
        showModName = showString . GHC.moduleNameString

makeInterface :: ModuleGoal -> Ghc ModuleInterface
makeInterface g = case g of
    SourceGoal tid -> do
        let obj_allowed = True
        head <$>  -- TODO
            moduleInterfacesForTargets [Target tid obj_allowed Nothing]
    ModuleGoal modName mPkgKey -> do
        packageModuleInterface modName mPkgKey


-- | Construct a `ModuleInterface`, given a `ModuleName` and optional
-- `PackageKey`. Without an explicitly given package, this will use
-- source modules found in the local path.
--
packageModuleInterface ::
    Interface.ModuleName -> Maybe PkgKey -> Ghc ModuleInterface
packageModuleInterface modName mPkgKey = do
    {- Note: Including the package key results in a failure to find module.
         Why doesn't this work?
    ghcModule <- GHC.lookupModule (mkModuleName modName) (fmap keyFS mPkgKey)
    -}

    ghcModule <- GHC.lookupModule (mkModuleName modName) Nothing -- XXX

    m <- getModuleInfo ghcModule
    case m of
        Nothing ->
            error $ "packageModuleInterface: failed to load module " ++ modName
        Just modInfo -> runLoadMod $ makeModuleInfoInterface ghcModule modInfo
  where
    keyFS :: PkgKey -> FastString
    keyFS = Module.packageKeyFS . pkgKeyToGHC


-- | Produce a `ModuleInterface` for each compilation target in
-- topological order.
moduleInterfacesForTargets ::
    [Target] ->    -- filepaths and/or module names
    Ghc [ModuleInterface]
moduleInterfacesForTargets targets = do
    setTargets targets

    moduleGraph <- depanal [] False

    when (needsTemplateHaskell moduleGraph) $
        error "readModuleInterface: Template Haskell unsupported"  -- TODO

    runLoadMod $
        forM (listModSummaries moduleGraph) $
            typecheckedModuleInterface <=<
                liftGHC . (typecheckModule <=< parseModule)
  where
    -- Sort the modules topologically and discard hs-boot modules.
    -- The topological order is not currently used, but it will be
    -- necessary in the future
    listModSummaries :: ModuleGraph -> [ModSummary]
    listModSummaries = filter (not . isBootSummary) . sortGraph

    sortGraph :: ModuleGraph -> ModuleGraph
    sortGraph g = flattenSCCs $ topSortModuleGraph False g Nothing


-- * LoadModule

data LoadModuleState = LMS
    { lmsTypeEnv       :: TypeEnv
        -- ^ type constructors from previously-loaded modules
    , lmsCurrentModule :: Maybe GHC.Module
        -- ^ the module that we are currently modeling an interface for
    , lmsExposedTyCons :: UniqSet GHC.TyCon
        -- ^ exposed type constructors that we have encountered so far
    , lmsOrigins       :: Map.Map SomeName Interface.Origin
        -- ^ origins for every entity
    }

initLMState :: LoadModuleState
initLMState = LMS emptyTypeEnv Nothing emptyUniqSet Map.empty


newtype LoadModule a = LoadModule (StateT LoadModuleState Ghc a)
    deriving (Functor, Applicative, Monad)

runLoadMod :: LoadModule a -> Ghc a
runLoadMod (LoadModule m) = evalStateT m initLMState

liftGHC :: Ghc a -> LoadModule a
liftGHC = LoadModule . lift

pprGHC :: (Out.Outputable a) => a -> LoadModule String
pprGHC a = do
    dflags <- liftGHC getDynFlags
    pure $ Out.showPpr dflags a

withTypeEnv :: State TypeEnv a -> LoadModule a
withTypeEnv m = LoadModule $ do
    te0 <- gets lmsTypeEnv
    let (a, te) = runState m te0
    a <$ modify (\lms -> lms {lmsTypeEnv = te})
    
-- | Set the module target for constructing a module interface. This resets
-- all state information pertaining to the previous module.
setTargetModule :: GHC.Module -> LoadModule ()
setTargetModule ghcModule =
    LoadModule $ modify $ \lms ->
        lms { lmsCurrentModule = Just ghcModule
            , lmsExposedTyCons = emptyUniqSet
            }

-- | Determine if the given named entity is defined in the current target
-- module.
isLocalThing :: (GHC.NamedThing a) => a -> LoadModule Bool
isLocalThing a = LoadModule $ do
    mGhcModule <- gets lmsCurrentModule
    pure $ case mGhcModule of
        Nothing -> False      -- without a module, there are no "local" names
        Just{} -> Name.nameModule_maybe (GHC.getName a) == mGhcModule

-- | Mark a `GHC.TyCon` as "seen", and create a `TypeConLink` that can be
-- used to construct a `Type`. 
--
-- After the module interface has been loaded, the `LoadModuleState` contains
-- a list of all encountered type constructors since the last call to
-- `setTargetModule`.
--
linkTyCon :: GHC.TyCon -> LoadModule TypeConLink
linkTyCon tyCon = do
    LoadModule $ modify $ \lms ->
        lms { lmsExposedTyCons = addOneToUniqSet (lmsExposedTyCons lms) tyCon }
    pure $ makeQualName $ GHC.getName tyCon


-- | Produce a list of every `GHC.TyCon` that has been "linked" since the
-- previous call to `setTargetModule`. See `linkTyCon`.
seenLocalTyCons :: LoadModule [GHC.TyCon]
seenLocalTyCons =
    filterM isLocalThing . uniqSetToList =<< LoadModule (gets lmsExposedTyCons)


-- | Store an `Interface.Origin` for the given entity. This should be called
-- for every entity that will be stored in the target `ModuleInterface`.
storeOrigin :: (GHC.NamedThing a) => Namespace -> a -> LoadModule ()
storeOrigin ns a = do
    LoadModule $ modify $ \lms ->
        lms { lmsOrigins = Map.insert name origin $ lmsOrigins lms }
  where
    name = SomeName ns (makeRawName a)
    origin = makeOrigin (GHC.getName a)


-- * Building ModuleInterfaces

typecheckedModuleInterface ::
    GHC.TypecheckedModule -> LoadModule ModuleInterface
typecheckedModuleInterface tcMod = do
    _ <- liftGHC $ loadModule tcMod
    let ghcModule = ms_mod . pm_mod_summary $ tm_parsed_module tcMod
    makeModuleInfoInterface ghcModule $ GHC.moduleInfo tcMod


makeModuleInfoInterface ::
    GHC.Module -> GHC.ModuleInfo -> LoadModule ModuleInterface
makeModuleInfoInterface ghcModule modInfo = do
    let modName = makeModuleName ghcModule

    setTargetModule ghcModule

    exports <- mapM loadExport (modInfoExports modInfo)

    let instances = []
    {-
    TODO: The following will fail for package modules.
    I need to find another way to load class instances.

    instances <- mapM loadClassInstance $ modInfoInstances modInfo
    -}

    -- Insert a `TypeCon` into the `TypeEnv` for each linked type constructor
    -- that is both defined in and exposed by the current module. Then, return
    -- the `TypeMap` for the current module so the it can be included in
    -- the `ModuleInterface`.
    typeCons <- mapM makeTypeCon =<< seenLocalTyCons
    typeMap <- withTypeEnv $ do
        modify $ \env -> foldr insertType env typeCons
        gets $ Interface.lookupModule modName

    pure $ makeModuleInterface modName typeMap exports instances
  where
    loadClassInstance :: GHC.ClsInst -> LoadModule ClassInstance
    loadClassInstance ci =
        ClassInstance (Name.getOccString $ InstEnv.is_cls_nm ci)
            <$> mapM makeType (InstEnv.is_tys ci)   -- instance types


loadExport :: GHC.Name -> LoadModule Export
loadExport ghcName = do
    isLocal <- isLocalThing ghcName
    mTyThing <- liftGHC $ GHC.lookupName ghcName
    case mTyThing of
        Nothing ->
            error $ "loadExport: failed to find name " ++ showQualName q
        Just thing
            | isLocal -> makeLocalExport thing
            | otherwise ->
                pure $ makeNamed ghcName $
                    ReExport (ghcNameModule ghcName) (tyThingNamespace thing)
--                pure . ReExport $ SomeName (tyThingNamespace thing) <$> q
  where
    q :: Qual RawName
    q = makeQualName ghcName


makeLocalExport :: GHC.TyThing -> LoadModule Export
makeLocalExport thing = makeNamed thing <$> case thing of
    ACoAxiom{} -> error "makeLocalExport: ACoAxiom unimplemented"
    AnId a ->                                       -- value
        mkValueDecl Identifier <$> makeType (idType a)
    AConLike (ConLike.RealDataCon dcon) ->          -- data constructor
        mkValueDecl (DataCon $ makeDataFields dcon)
            <$> makeType (dataConType dcon)
            -- DataCon (makeType $ dataConType dcon) (makeDataConList dcon)
    AConLike (ConLike.PatSynCon patsyn) ->          -- pattern synonym
        mkValueDecl PatternSyn <$> makeType (PatSyn.patSynType patsyn)
    ATyCon tyCon
        | Just rhs <- synTyConRhs_maybe tyCon ->    -- type synonyms
            mkTypeDecl tyCon . TypeSyn =<< pprGHC rhs
        | isClassTyCon tyCon ->                     -- class definitions
            mkTypeDecl tyCon TypeClass
        | otherwise ->                              -- data/newtype/other
            mkTypeDecl tyCon $ Interface.DataType (makeDataConList tyCon)
  where
    mkValueDecl :: ValueDeclInfo -> Interface.Type -> Entity
    mkValueDecl i t = LocalValue $ ValueDecl t i

    mkTypeDecl :: GHC.TyCon -> TypeDeclInfo -> LoadModule Entity
    mkTypeDecl tyCon info =
        let kind = makeKind $ TyCon.tyConKind tyCon
        in pure $ LocalType $ TypeDecl kind info

    makeDataFields :: GHC.DataCon -> [DataField]
    makeDataFields = map mkField . dataConFieldLabels
      where
        mkField lbl = makeNamed lbl ()


-- | Construct a `Interface.Type` to be included in a `ModuleInterface`.
makeType :: GHC.Type -> LoadModule Interface.Type
makeType = go
  where
    go t0 = case splitForAllTys t0 of
        ([], t)
            | Just tyVar <- Type.getTyVar_maybe t ->
                pure $ var $ makeTypeVar tyVar
            | Just (ta, tb) <- Type.splitFunTy_maybe t ->
                if Type.isPredTy ta
                    then context <$> makePreds ta <*> go tb
                    else fun <$> go ta <*> go tb
            | Just (ta, tb) <- Type.splitAppTy_maybe t ->
                apply1 <$> go ta <*> go tb
            | Just (tc, ts) <- Type.splitTyConApp_maybe t -> do
                link <- linkTyCon tc
                apply (con link) <$> mapM go ts
            | otherwise -> do
                s <- pprGHC t
                fail $ "makeType: " ++ s
            -- TODO: numeric and string literals
        (vs, t) -> forall (map makeTypeVar vs) <$> go t

    makeTypeVar :: GHC.TyVar -> TypeVar
    makeTypeVar tyVar =
        TypeVar (Name.getOccString tyVar) (makeKind $ Var.tyVarKind tyVar)


-- | Construct a list of type predicates from a `GHC.PredType`
makePreds :: GHC.PredType -> LoadModule [Pred]
makePreds pt = case Type.classifyPredType pt of
    Type.ClassPred cls ts ->
        sequence
            [ Interface.ClassPred (makeQualName $ GHC.getName cls)
                <$> mapM makeType ts ]
    Type.EqPred rel a b ->
        sequence
            [ Interface.EqPred (makeEqRel rel) <$> makeType a <*> makeType b ]
    Type.TuplePred ps -> concat <$> mapM makePreds ps
    Type.IrredPred p ->
        error $ "makeTypes: IrredPred: " ++ unsafeOutput p
  where
    makeEqRel :: Type.EqRel -> Interface.EqRel
    makeEqRel r = case r of
        Type.NomEq  -> Interface.NomEq
        Type.ReprEq -> Interface.ReprEq


-- | Construct a `TypeCon` to be included in a `ModuleInterface`, and add it
-- to the `TypeEnv` when it originates in the current module.
makeTypeCon :: GHC.TyCon -> LoadModule (Qual Interface.TypeCon)
makeTypeCon ghcTyCon = do
    let ghcName = GHC.getName ghcTyCon
    typeCon <-
        TypeCon (makeRawName ghcName) (makeOrigin ghcName) kind <$> info

    -- traceM $ "TRACE makeTypeCon: " ++ show typeCon

    let namedCon = makeQual ghcName typeCon

    withTypeEnv $ namedCon <$ do
        let name = Interface.getQualName namedCon
        mStoredCon <- gets $ lookupType name
        case mStoredCon of
            Left _ ->  -- the type constructor is not stored yet: add it
                modify $ insertType namedCon
            Right storedCon
                | storedCon /= typeCon ->
                    fail $ "makeTypeCon: type constructor does not match the"
                        ++ " one in TypeEnv: " ++ showQualName name
                {- TODO: ^ This asserts that the now-created type constructor is
                   the same as the one stored in the TypeEnv.
                   They should _always_ be the same, and this will later be
                   removed.
                -}
                | otherwise -> pure ()
  where
    kind = makeKind $ TyCon.tyConKind ghcTyCon

    info
        | Just _cls <- TyCon.tyConClass_maybe ghcTyCon =
            pure ConClass
        | TyCon.isAlgTyCon ghcTyCon =
            pure ConAlgebraic
        | TyCon.isTypeSynonymTyCon ghcTyCon =
            pure ConSynonym
        -- TODO: type family instances
        | TyCon.isFamInstTyCon ghcTyCon =
            fail "makeTypeCon: family instances not implemented"
        | otherwise = do
            s <- pprGHC ghcTyCon
            fail $ "makeTypeCon: unrecognized: " ++ s

    --  | isFunTyCon tc = 
    --  | isPrimTyCon tc = 
    --  | isTupleTyCon tc = 
    --  | isUnboxedTupleTyCon tc = 
    --  | isBoxedTupleTyCon tc = 
    --  | isPromotedDataCon tc = 


makeDataConList :: GHC.TyCon -> DataConList
makeDataConList tc
    | TyCon.isAbstractTyCon tc = Abstract
    | otherwise = DataConList $ map makeRawName (tyConDataCons tc)


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
makeRawName = Name.getOccString . GHC.getName


tyThingNamespace :: GHC.TyThing -> Interface.Namespace
tyThingNamespace tyThing = case tyThing of
    AnId{}     -> Values
    AConLike{} -> Values
    ATyCon{}   -> Types
    ACoAxiom{} -> Types


makeNamed :: (GHC.NamedThing n) => n -> a -> Named a
makeNamed n = Named (makeRawName ghcName)
  where
    ghcName = GHC.getName n


ghcNameModule :: GHC.Name -> Interface.ModuleName
ghcNameModule = makeModuleName . GHC.nameModule

makeQual :: GHC.Name -> a -> Qual a
makeQual = Interface.Qual . ghcNameModule


makeQualName :: GHC.Name -> Qual RawName
makeQualName n = makeQual n (makeRawName n)


makeQualNamed :: GHC.Name -> a -> Qual (Named a)
makeQualNamed n = makeQual n . makeNamed n


makeOrigin :: GHC.Name -> Origin
makeOrigin ghcName = case nameSrcSpan ghcName of
    RealSrcSpan ss ->
        let path = unpackFS $ srcSpanFile ss
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
