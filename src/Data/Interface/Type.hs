{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Interface.Type where

import Debug.Trace


data a :-> b = a :-> b
    deriving (Show, Read, Eq, Ord, Functor)


data TypeVar = TypeVar String Kind
    deriving (Show, Eq, Ord)

varName :: TypeVar -> String
varName (TypeVar n _) = n

varKind :: TypeVar -> Kind
varKind (TypeVar _ k) = k


data Type
    = Type String Kind
    | Var TypeVar
    | AppType Type Type
    | FunType (Type :-> Type)
    | Forall [TypeVar] Type
    deriving (Show, Eq, Ord)

typeKind :: Type -> Kind
typeKind t0 = case t0 of
    Type _ k -> k
    Var v -> varKind v
    AppType f _ ->
        let k0 = typeKind f
            t = "TRACE typeKind: not FunKind: " ++ show k0
        in case resultKind k0 of
            Nothing -> trace t k0
            Just k -> k
    FunType (a :-> b) -> StarKind
    Forall _ t -> typeKind t

showsTypePrec :: Int -> Type -> ShowS
showsTypePrec p t0 = case t0 of
    Type s _ -> showString s
    Var v -> showString (varName v)
    AppType a b -> showParen (p > 10) $
        showsTypePrec 10 a . showChar ' ' . showsTypePrec 10 b
    FunType (a :-> b) -> showParen (p > 10) $
        showsTypePrec 10 a . showString infixString . showsTypePrec 10 b
          where
            infixString = case typeKind a of
                ConstraintKind -> " => "
                _              -> " -> "
    Forall vs t -> showString (unwords $ "forall" : map varName vs) .
                   showString ". " . showsTypePrec 0 t

showType :: Type -> String
showType t = showsTypePrec 0 t ""

isPromotable :: Type -> Bool
isPromotable t = case t of
    FunType (a :-> b) -> isStar a && isPromotable b
    _ -> isStar t
  where
    isStar a = StarKind == typeKind a
    

promote :: Type -> Maybe Kind
promote t
    | isPromotable t = Just $ PromotedType t
    | otherwise      = Nothing


data Kind
    = KindVar String
    | StarKind                    -- ^ Lifted types (*)
    | HashKind                    -- ^ Unlifted types (#)
    | SuperKind                   -- ^ the type of kinds (BOX)
    | ConstraintKind              -- ^ Constraints
    | PromotedType Type           -- ^ promoted type using DataKinds
    | FunKind (Kind :-> Kind)
    deriving (Show, Eq, Ord)

{- Kind notes:
     TODO: GHC also has BOX, AnyK, and OpenKind
-}

-- | Determine the result of a `FunKind`. This is a partial function.
resultKind :: Kind -> Maybe Kind
resultKind k0 = case k0 of
    FunKind (_ :-> k) -> Just k
    _ -> Nothing


showsKind :: Kind -> ShowS
showsKind k = case k of
    KindVar s -> showString s
    StarKind -> showChar '*'
    HashKind -> showChar '#'
    SuperKind -> showString "BOX"
    ConstraintKind -> showString "Constraint"
    PromotedType t -> showString "[showsKind: ERROR PromotedType TODO]"
    FunKind (ka :-> kr) -> showsKind ka . showString " -> " . showsKind kr

showKind :: Kind -> String
showKind k = showsKind k ""

