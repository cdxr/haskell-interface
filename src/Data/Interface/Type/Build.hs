module Data.Interface.Type.Build where

import Data.Interface.Type


con :: TypeConLink -> Type
con = Con

apply1 :: Type -> Type -> Type
apply1 = Apply

-- | Apply a type to several argument types.
--
-- @apply c ts@ requires that the arity of @c@ is at least @length ts@.
apply :: Type -> [Type] -> Type
apply = foldl apply1

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


{-
 - TODO applyFun:
 -
-- | Construct nested function applications from the given types.
--
-- @applyFun []      =>  (->)@
-- @applyFun [x]     =>  (x ->)@
-- @applyFun [x,y]   =>  (x -> y)@
-- @applyFun [x,y,z] =>  (x -> y -> z)@
applyFun :: [Type] -> Type
applyFun [] = 
-}
