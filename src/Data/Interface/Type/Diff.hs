{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Interface.Type.Diff where

import qualified Data.Functor.Foldable as FF

import Data.Interface.Change
import Data.Interface.Type.Type


-- | @DiffTypeF t c@ represents a potential change to a @TypeF t@ where @c@
-- is a potential change to an @t@.
--
-- @t@ is a single type
-- @c@ is a type change (containing a pair of potentially-different types)
data DiffTypeF t c
    = DiffTypeF (Replace (TypeF t))   -- ^ TypeF is changed
    | SameTypeF (TypeF c)             -- ^ TypeF is not changed (@a@ might be)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Diff a c) => Diff (TypeF a) (DiffTypeF a c) where
    diff = diffTypeF

    toChange d = case d of
        SameTypeF fc                           -- if the TypeFs are the same,
            | Just a <- traverse same fc ->    --   check to see if each child
                Same a                         --   is the same.
            | otherwise ->                     -- if not, reconstruct the
                Change                         --   and wrap them in the
                    (fmap old fc)              --   `Change` constructor
                    (fmap new fc)
        DiffTypeF r -> toChange r


-- | Compare two open type terms.
diffTypeF :: (Diff a c) => TypeF a -> TypeF a -> DiffTypeF a c
diffTypeF a b = case (a, b) of
    (ConF c0, ConF c1) | c0 == c1 ->
        SameTypeF $ ConF c1
    (ApplyF c0 a0, ApplyF c1 a1) ->
        SameTypeF $ ApplyF (diff c0 c1) (diff a0 a1)
    (FunF a0 b0, FunF a1 b1) ->
        SameTypeF $ FunF (diff a0 a1) (diff b0 b1)
    (VarF v0, VarF v1) | v0 == v1 ->
        SameTypeF $ VarF v1
    (ForallF vs0 t0, ForallF vs1 t1) | vs0 == vs1 ->
        SameTypeF $ ForallF vs1 (diff t0 t1)
    (ContextF ps0 t0, ContextF ps1 t1) | ps0 == ps1 ->
        SameTypeF $ ContextF ps1 (diff t0 t1)
    _ -> DiffTypeF (Replace a b)

{- diffTypeF notes:
     - Type constructors are considered equal if they have the same name and
       origin.
     - Contexts are only considered equal if they have the same predicates
       with the same variables in exactly the same order.
       This should be improved.
     - Type variables are compared for literal equality. Changes will have
       to be made to the TypeF type in order to compare for alpha-equality.
-}


type TypeDiff = FF.Fix (DiffTypeF Type)

-- | @iterTypeDiff td@ is either @Left@ two different Types, or @Right@
-- an open type term that matches both types, containing additional TypeDiffs.
iterTypeDiff :: TypeDiff -> Either (Replace Type) (TypeF TypeDiff)
iterTypeDiff td = case FF.project td of
    DiffTypeF r -> Left $ fmap FF.embed r
    SameTypeF t -> Right $ t


pattern TypeDiff :: DiffTypeF Type TypeDiff -> TypeDiff
pattern TypeDiff f = FF.Fix f

instance Diff Type TypeDiff where
    diff a b = FF.embed $ diffTypeF (FF.project a) (FF.project b)
    toChange = fmap FF.embed . toChange . FF.project
