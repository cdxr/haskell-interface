{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Interface.Type.Type where

import Data.Interface.Name


data Type
    = Con (Qual (Named TypeCon))  -- ^ type constructors
    | Link (Qual TypeName)        -- ^ "links" to type constructors
    | Apply Type Type             -- ^ type constructor application
    | Fun Type Type               -- ^ (->) type constructor
    | Var TypeVar                 -- ^ type variables ("a")
    | Forall [TypeVar] Type       -- ^ forall qualifiers / constraints
    deriving (Show, Eq, Ord)

type Constraint = Type


applyType :: Type -> [Type] -> Type
applyType t [] = t
applyType t (t':ts) = Apply t t' `applyType` ts



data TypeVar = TypeVar String Kind
    deriving (Show, Eq, Ord)

varName :: TypeVar -> String
varName (TypeVar n _) = n

varKind :: TypeVar -> Kind
varKind (TypeVar _ k) = k


data TypeCon = TypeCon TypeConInfo Kind
    deriving (Show, Eq, Ord)

-- | The introducer of a type constructor
data TypeConInfo
    = ConAlgebraic   -- ^ data/newtype declaration
    | ConSynonym     -- ^ type synonym
    | ConClass       -- ^ class declaration
    deriving (Show, Eq, Ord)

type instance Space TypeCon = 'Types



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
