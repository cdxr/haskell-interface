module Data.Interface.Type.Build where

import Data.Interface.Type


con :: TypeConLink -> Type
con = Con

apply1 :: Type -> Type -> Type
apply1 a b = case a of
    Apply (ConApply c cs) -> apply c (cs ++ [b])  -- TODO (this is O^2)
    _                     -> apply a [b]


-- | Apply a type to several argument types.
--
-- @apply c ts@ requires that the arity of @c@ is at least @length ts@.
apply :: Type -> [Type] -> Type
apply t0 [] = t0
apply t0 ts = Apply $ case (typeName t0, ts) of
    (Just "[]", [t]) -> ConList t
    (Just "(,)", [_,_]) -> ConTuple 2 ts
    (Just "(,,)", [_,_,_]) -> ConTuple 3 ts
    (Just "(,,,)", [_,_,_,_]) -> ConTuple 4 ts
    (Just "(,,,,)", [_,_,_,_,_]) -> ConTuple 5 ts
    _ -> ConApply t0 ts


fun :: Type -> Type -> Type
fun = Fun

var :: TypeVar -> Type
var = Var

forall :: [TypeVar] -> Type -> Type
forall = Forall

context :: [Pred Type] -> Type -> Type
context ps t0 = case t0 of
    Context ps' t -> Context (ps ++ ps') t      -- merge nested contexts
    _ -> Context ps t0
