{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Interface.Type.Diff where

import qualified Data.Functor.Foldable as FF

import Data.Interface.Change

import Data.Interface.Type.Type


-- TODO: rewrite diffTopTypeF in terms of maybeSameTypeF

-- | @maybeSameTypeF t0 t1@ is @Just t@ when the topmost terms of @t0@ and
-- @t1@ are considered equal.
maybeSameTypeF :: (Eq t, Diff t c) => TypeF t -> TypeF t -> Maybe (TypeF c)
maybeSameTypeF a b = case (a, b) of
    (ConF c0, ConF c1)
        | c0 == c1 -> Just $ ConF c1
    (ApplyF a0, ApplyF a1)
        | a0 == a1 -> Just $ fmap noDiff b
    (FunF a0 b0, FunF a1 b1)
        | a0 == a1 -> Just $ FunF (noDiff a0) (diff b0 b1)
    (VarF v0, VarF v1)
        | v0 == v1 -> Just $ VarF v1
    (ForallF vs0 t0, ForallF vs1 t1)
        | vs0 == vs1 -> Just $ ForallF vs1 (diff t0 t1)
    (ContextF ps0 t0, ContextF ps1 t1)
        | ps0 == ps1 -> Just $ ContextF ps1 (diff t0 t1)
    _ -> Nothing



-- | @DiffTypeF t c@ represents a potential change to a @TypeF t@ where @c@
-- is a potential change to an @t@.
--
-- @t@ is a single type
-- @c@ is a type change (containing a pair of potentially-different types)
data DiffTypeF t c
    = NoDiffTypeF (TypeF t)              -- ^ entire type is unchanged
    | ReplaceTypeF (TypeF t) (TypeF t)   -- ^ topmost term is changed
    | SameTypeF (TypeF c)                -- ^ topmost term is not changed
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- DiffTypeF
 -  invariant: ReplaceTypeF contains changes somewhere
-}



instance (ToChange a c) => ToChange (TypeF a) (DiffTypeF a c) where
    toChange d = case d of
        NoDiffTypeF t -> NoChange t
        ReplaceTypeF a b -> Change a b
        SameTypeF fc -> traverse toChange fc

    isChanged d = case d of
        NoDiffTypeF{}  -> False
        ReplaceTypeF{} -> True
        SameTypeF{}    -> True  -- invariant: SameTypeF contains changes

instance (Eq a, Diff a c) => Diff (TypeF a) (DiffTypeF a c) where
    diff a0 b0 = case diffTopTypeF a0 b0 of
        dt@(SameTypeF (FunF a b)) | isChanged a && isChanged b -> 
            let Replace a b = toReplace dt
            in ReplaceTypeF a b
        dt -> dt
    {-# INLINABLE diff #-}

    noDiff = NoDiffTypeF
    {-# INLINABLE noDiff #-}


{- diff notes:
     - Type constructors are considered equal if they have the same name and
       module of origin.
     - Contexts are only considered equal if they have the same predicates
       with the same variables in exactly the same order.
       This should be improved.
     - Type variables are compared for literal equality. Changes will have
       to be made to the TypeF type in order to compare for alpha-equality.
-}


-- | The first step to comparing a Type. The very top TypeF constructors are
-- compared directly for equality, without regard to lower terms.
diffTopTypeF :: (Eq a, Diff a c) => TypeF a -> TypeF a -> DiffTypeF a c
diffTopTypeF a b = case (a, b) of
    (ConF c0, ConF c1) | c0 == c1 ->
        sameTopTerm $ ConF c1
    (ApplyF a0, ApplyF a1) | a0 == a1 -> NoDiffTypeF b
    (FunF a0 b0, FunF a1 b1) ->
        sameTopTerm $ FunF (diff a0 a1) (diff b0 b1)
    (VarF v0, VarF v1) | v0 == v1 ->
        sameTopTerm $ VarF v1
    (ForallF vs0 t0, ForallF vs1 t1) | vs0 == vs1 ->
        sameTopTerm $ ForallF vs1 (diff t0 t1)
    (ContextF ps0 t0, ContextF ps1 t1) | ps0 == ps1 ->
        sameTopTerm $ ContextF ps1 (diff t0 t1)
    _ -> ReplaceTypeF a b
  where
    sameTopTerm :: (Diff a c) => TypeF c -> DiffTypeF a c
    sameTopTerm t0
        | Just t <- traverse getNoDiff t0 =
            NoDiffTypeF t    -- the entire type is unchanged
        | otherwise =
            SameTypeF t0    -- only the topmost term is unchanged



type TypeDiff = FF.Fix (DiffTypeF Type)

-- | @iterTypeDiff td@ is either @Left@ two different Types, or @Right@
-- an open type term that matches both types, containing additional TypeDiffs.
iterTypeDiff :: TypeDiff -> Either (Replace Type) (TypeF TypeDiff)
iterTypeDiff td = case FF.project td of
    NoDiffTypeF t -> Right (fmap noDiff t)
    ReplaceTypeF a b -> Left $ Replace (FF.embed a) (FF.embed b)
    SameTypeF t -> Right t


pattern TypeDiff :: DiffTypeF Type TypeDiff -> TypeDiff
pattern TypeDiff f = FF.Fix f

instance ToChange Type TypeDiff where
    toChange = fmap FF.embed . toChange . FF.project

instance Diff Type TypeDiff where
    diff a b = FF.embed $ diff (FF.project a) (FF.project b)
    noDiff = FF.embed . noDiff . FF.project



-- | A `TypeDiff` extended with constructors for specific cases
data TypeDiff'F c
    = TypeDiff'F (DiffTypeF Type c)
    | ChangeContext (ChangeContext Type) c
    deriving (Show, Eq, Ord, Functor)

data TypeDiff' = TypeDiff' (TypeDiff'F TypeDiff')
    deriving (Show, Eq, Ord)

data ChangeContext t
    = RemovedContext [Pred t]
    | AddedContext [Pred t]
    | ContextElems (Patience (Pred t))
    deriving (Show, Eq, Ord)

changePreds :: ChangeContext t -> Change [Pred t]
changePreds cc = case cc of
    RemovedContext ps -> Change ps []
    AddedContext ps   -> Change [] ps
    ContextElems es   -> toChange es

type instance FF.Base TypeDiff' = TypeDiff'F

instance FF.Foldable TypeDiff' where
    project (TypeDiff' a) = a

instance FF.Unfoldable TypeDiff' where
    embed = TypeDiff'


instance ToChange Type TypeDiff' where
    toChange (TypeDiff' td0) = case td0 of
        TypeDiff'F fd -> fmap FF.embed $ toChange fd
        ChangeContext cc (TypeDiff' td) ->
            Context <$> changePreds cc <*> toChange (FF.embed td)

instance Diff Type TypeDiff' where
    diff t0 t1 = extendTypeDiff $ diff t0 t1

        
{-
revertTypeDiff :: TypeDiff' -> TypeDiff
revertTypeDiff = FF.cata revertSpecialCases
  where
    revertSpecialCases :: TypeDiff'F TypeDiff -> TypeDiff
    revertSpecialCases td = case td of
        ChangeContext cc c -> case cc of
            RemovedContext ps -> ReplaceTypeF _ c
-}


extendTypeDiff :: TypeDiff -> TypeDiff'
extendTypeDiff = FF.cata applySpecialCases
  where
    applySpecialCases :: DiffTypeF Type TypeDiff' -> TypeDiff'
    applySpecialCases df = FF.embed $ case df of
        -- Context has been changed, added, or removed
        ReplaceTypeF (ContextF ps0 t0) (ContextF ps1 t1)
            | t0 `almostEqual` t1 ->
                ChangeContext (ContextElems $ diff ps0 ps1) (diff t0 t1)
        ReplaceTypeF (ContextF ps0 t0) (FF.embed -> t1)
            | t0 `almostEqual` t1 ->
                ChangeContext (RemovedContext ps0) (diff t0 t1)
        ReplaceTypeF (FF.embed -> t0) (ContextF ps1 t1)
            | t0 `almostEqual` t1 ->
                ChangeContext (AddedContext ps1) (diff t0 t1)

        -- None of the above cases
        _ -> TypeDiff'F df

    almostEqual :: Type -> Type -> Bool
    almostEqual = (==)  -- TODO


stripForallDiff :: DiffTypeF Type TypeDiff -> DiffTypeF Type TypeDiff
stripForallDiff td = case td of
    NoDiffTypeF t -> NoDiffTypeF $ stripTerm t
    ReplaceTypeF t0 t1 -> ReplaceTypeF (stripTerm t0) (stripTerm t1)
    SameTypeF (ForallF _ c) -> FF.project c
    _ -> td
  where
    stripTerm :: TypeF Type -> TypeF Type
    stripTerm = FF.project . stripForall . FF.embed


stripForallDiff' :: TypeDiff' -> TypeDiff'
stripForallDiff' (TypeDiff' td0) = case td0 of
    TypeDiff'F (NoDiffTypeF t) ->
        TypeDiff' $ TypeDiff'F $ NoDiffTypeF $ stripTerm t
    TypeDiff'F (ReplaceTypeF t0 t1) ->
        TypeDiff' $ TypeDiff'F $ diff (stripTerm t0) (stripTerm t1)
    TypeDiff'F (SameTypeF (ForallF _ c)) -> c
    _ -> TypeDiff' td0
  where
    stripTerm :: TypeF Type -> TypeF Type
    stripTerm = FF.project . stripForall . FF.embed
