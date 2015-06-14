{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Interface.Type where


type Type = String      -- ^ TODO

promote :: Type -> Maybe Kind
promote = undefined


data a :-> b = a :-> b
    deriving (Show, Read, Eq, Ord, Functor)

data Kind
    = StarKind                    -- ^ Lifted types (*)
    | HashKind                    -- ^ Unlifted types (#)
    | ConstraintKind              -- ^ Constraints (Constraint)
    | PromotedType Type Kind      -- ^ promoted type using DataKinds
    | ApplyKind (Kind :-> Kind)
    deriving (Show, Read, Eq, Ord)

{- Kind notes:
     TODO: GHC also has BOX, AnyK, and OpenKind
-}

showsKind :: Kind -> ShowS
showsKind k = case k of
    StarKind -> showChar '*'
    HashKind -> showChar '#'
    ConstraintKind -> showString "Constraint"
    PromotedType t k -> showString "[showsKind: ERROR PromotedType TODO]"
    ApplyKind (ka :-> kr) -> showsKind ka . showString " -> " . showsKind kr

showKind :: Kind -> String
showKind k = showsKind k ""

