{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Interface.Type.Type where

import Data.Interface.Name
import Data.Interface.Source ( Origin )


data Type
    = Con (Qual TypeCon)          -- ^ type constructors
    | Link (Qual TypeName)        -- ^ "links" to type constructors
    | Apply Type Type             -- ^ type constructor application
    | Fun Type Type               -- ^ (->) type constructor
    | Var TypeVar                 -- ^ type variables ("a")
    | Forall [TypeVar] Type       -- ^ forall qualifiers / constraints
    | Context [Pred] Type         -- ^ class and equality predicates
    deriving (Show, Eq, Ord)


-- | A class or equality predicate
data Pred
    = ClassPred (Qual RawName) [Type]          
    | EqPred EqRel Type Type
    deriving (Show, Eq, Ord)

-- | A choice of equality relation. Copied from GHC.Type.
data EqRel = NomEq | ReprEq
    deriving (Show, Eq, Ord)


applyType :: Type -> [Type] -> Type
applyType t [] = t
applyType t (t':ts) = Apply t t' `applyType` ts


{- TODO: normalizeType:
 -   It would be nice to have a representation geared towards convenient
 -   construction, and `normalizeType` would return a different type that
 -   was guaranteed to be in normalized form, and optimized for comparison
 -   and display.
 -
 -   This might be a good place to scrap my boilerplate...
 -}
normalizeType :: Type -> Type
normalizeType t0 = case t0 of
    Context ps (Context ps' t) -> Context (ps ++ ps') $ normalizeType t
    Context ps t -> Context ps $ normalizeType t
    Forall vs (Forall vs' t) -> Forall (vs ++ vs') $ normalizeType t
    Forall vs t -> Forall vs $ normalizeType t
    Fun a b -> Fun (normalizeType a) (normalizeType b)
    Apply a b -> Apply (normalizeType a) (normalizeType b)
    Con{} -> t0
    Link{} -> t0
    Var{} -> t0


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
    , typeConIntro  :: TypeConIntro
    } deriving (Show, Eq, Ord)

-- | The introducer of a type constructor
data TypeConIntro
    = ConAlgebraic   -- ^ data/newtype declaration
    | ConSynonym     -- ^ type synonym
    | ConClass       -- ^ class declaration
    deriving (Show, Eq, Ord)

type instance Space TypeCon = 'Types

instance HasRawName TypeCon where
    rawName = typeConName

instance HasNamespace TypeCon where
    namespace _ = Types



type Arity = Int

data WiredType
    = WBool
    | WEq
    | WOrdering
    | WChar
    | WDouble
    | WFloat
    | WInt
    | WWord
    | WList
    | WTuple Arity
    | WUnit
    deriving (Show, Eq, Ord)


data Kind
    = KindVar String
    | StarKind                     -- ^ Lifted types (*)
    | HashKind                     -- ^ Unlifted types (#)
    | SuperKind                    -- ^ the type of kinds (BOX)
    | ConstraintKind               -- ^ Constraints
    | PromotedType (Qual TypeName) -- ^ promoted type using DataKinds
    | FunKind Kind Kind
    deriving (Show, Eq, Ord)

{- Kind notes:
     TODO: GHC also has BOX, AnyK, and OpenKind
-}

-- | Determine the result of a `FunKind`. This is a partial function.
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


-- | The kind of a basic type constructor.
basicTypeConKind :: Arity -> Kind
basicTypeConKind a
    | a < 0 = error "basicTypeConKind: negative arity"
    | otherwise = go a
  where
    go 0 = StarKind
    go n = FunKind StarKind $ go (n-1)
