{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Interface.Type.Type where

import qualified Data.Functor.Foldable as FF

import Data.Interface.Name
import Data.Interface.Source ( Origin )


data Type
    = Con TypeConLink             -- ^ type constructors
    | Apply (TypeApply Type)      -- ^ type constructor application
    | Fun Type Type               -- ^ (->) type constructor
    | Var TypeVar                 -- ^ type variables ("a")
    | Forall [TypeVar] Type       -- ^ forall qualifiers / constraints
    | Context [Pred Type] Type    -- ^ class and equality predicates
    deriving (Show, Eq, Ord)


-- | The open-recursion form of `Type` that enables composition with other
-- functors. See `Data.Interface.Type.Diff` for how this is used to construct
-- a tree of type differences.
data TypeF a
    = ConF TypeConLink             -- ^ type constructors
    | ApplyF (TypeApply a)         -- ^ type constructor application
    | FunF a a                     -- ^ (->) type constructor
    | VarF TypeVar                 -- ^ type variables ("a")
    | ForallF [TypeVar] a          -- ^ forall qualifiers / constraints
    | ContextF [Pred Type] a       -- ^ class and equality predicates
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type instance FF.Base Type = TypeF

instance FF.Foldable Type where
    project t0 = case t0 of
        Con l -> ConF l
        Apply a -> ApplyF a
        Fun a b -> FunF a b
        Var v -> VarF v
        Forall vs t -> ForallF vs t
        Context ps t -> ContextF ps t

instance FF.Unfoldable Type where
    embed f = case f of
        ConF l -> Con l
        ApplyF a -> Apply a
        FunF a b -> Fun a b
        VarF v -> Var v
        ForallF vs t -> Forall vs t
        ContextF ps t -> Context ps t

instance TraverseNames Type where
    traverseNames f = fmap FF.embed . traverseNames f . FF.project

-- | only includes type constructors
instance (TraverseNames a) => TraverseNames (TypeF a) where
    traverseNames f t0 = case t0 of
        ConF c -> ConF <$> traverseNames f c
        ContextF ps t ->
            ContextF <$> traverse (traverseNames f) ps <*> traverseNames f t
        _ -> traverse (traverseNames f) t0


-- | A "link" to a type constructor.
--
-- When performing diff analysis for a type, identical TypeConLinks indicate
-- a form of "shallow" type equality. Depending on the origin of the links,
-- it may be desirable to look up the TypeCon referenced by the link in order
-- to measure "deep" type equality.
type TypeConLink = Qual RawName


-- | A class or equality predicate
data Pred a
    = ClassPred [a]          
    | EqPred EqRel a a
    deriving (Show, Eq, Ord, Functor)

-- | A choice of equality relation. Copied from GHC.Type.
data EqRel = NomEq | ReprEq
    deriving (Show, Eq, Ord)

instance (TraverseNames a) => TraverseNames (Pred a) where
    traverseNames f p = case p of
        ClassPred ts ->
            ClassPred <$> traverse (traverseNames f) ts
        EqPred r a b ->
            EqPred r <$> traverseNames f a <*> traverseNames f b


data TypeVar = TypeVar String Kind
    deriving (Show, Eq, Ord)

varName :: TypeVar -> String
varName (TypeVar n _) = n

varKind :: TypeVar -> Kind
varKind (TypeVar _ k) = k


data TypeCon = TypeCon
    { typeConName   :: RawName
    , typeConOrigin :: Origin
    , typeConKind   :: Kind
    , typeConInfo   :: TypeConInfo
    } deriving (Show, Eq, Ord)


data TypeConInfo
    = ConAlgebraic   -- ^ data/newtype declaration
    | ConSynonym     -- ^ type synonym
    | ConClass       -- ^ class declaration
    deriving (Show, Eq, Ord)

type instance Space TypeCon = 'Types

instance HasRawName TypeCon where
    rawName = typeConName
    rename f tcon = tcon { typeConName = f (typeConName tcon) }

instance HasNamespace TypeCon where
    namespace _ = Types

instance TraverseNames TypeCon where
    traverseNames f (TypeCon n o k i) =
        TypeCon <$> f n
                <*> pure o
                <*> traverseNames f k
                <*> pure i


typeName :: Type -> Maybe RawName
typeName t0 = case t0 of
    Con l -> Just $ rawName l
    _     -> Nothing


stripForall :: Type -> Type
stripForall t0 = case FF.project t0 of
    ForallF _ t -> t
    _           -> t0


data TypeApply t
    = ConApply t [t]
    | ConList t
    | ConTuple Int [t]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

takeApply :: Type -> Maybe (TypeApply Type)
takeApply t0 = case t0 of
    Apply a -> Just a
    _       -> Nothing



data Kind
    = KindVar String
    | StarKind                     -- ^ Lifted types (*)
    | HashKind                     -- ^ Unlifted types (#)
    | SuperKind                    -- ^ the type of kinds (BOX)
    | ConstraintKind               -- ^ Constraints
    | PromotedType (Qual RawName)  -- ^ promoted type using DataKinds
    | FunKind Kind Kind
    deriving (Show, Eq, Ord)

{- Kind notes:
     TODO: GHC also has AnyK and OpenKind
-}

instance TraverseNames Kind where
    traverseNames f k = case k of
        PromotedType q -> PromotedType <$> traverseNames f q
        FunKind k0 k1  -> FunKind <$> traverseNames f k0 <*> traverseNames f k1
        _ -> pure k


-- | Determine the codomain of a `FunKind`.
resultKind :: Kind -> Maybe Kind
resultKind k0 = case k0 of
    FunKind _ k -> Just k
    _ -> Nothing


showsKind :: Kind -> ShowS
showsKind k = case k of
    KindVar s -> showString s
    StarKind -> showChar '*'
    HashKind -> showChar '#'
    SuperKind -> showString "BOX"
    ConstraintKind -> showString "Constraint"
    PromotedType t -> showString "[showsKind: ERROR PromotedType TODO]"
    FunKind ka kr -> showsKind ka . showString " -> " . showsKind kr

showKind :: Kind -> String
showKind k = showsKind k ""


type Arity = Int

-- | The kind of a basic type constructor.
basicTypeConKind :: Arity -> Kind
basicTypeConKind a
    | a < 0 = error "basicTypeConKind: negative arity"
    | otherwise = go a
  where
    go 0 = StarKind
    go n = FunKind StarKind $ go (n-1)
