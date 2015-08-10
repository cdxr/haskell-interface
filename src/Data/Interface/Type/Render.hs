{-# LANGUAGE DeriveFunctor #-}

module Data.Interface.Type.Render
(
    renderTypeUnqual
  -- , renderTypeQual

  , TypeSig(..)
  , typeSig
  , renderTypeSig
) where

import Data.Monoid
import Data.List ( intersperse )
import Data.Functor.Foldable ( cata, embed )

import Data.Interface.Name ( rawName )
import Data.Interface.Type ( Type, TypeF(..),
                             TypeConLink, TypeVar(..), Pred(..) )
import Data.Interface.Type.Diff


data TypeSig a
    = Con a
    | Parens (TypeSig a)
    | Tupled [TypeSig a]
    | Apply [TypeSig a]
    | Fun [TypeSig a]
    | Var TypeVar
    | Forall [TypeVar] (TypeSig a)
    | Context [Pred (TypeSig a)] (TypeSig a)
    deriving (Functor)


renderTypeSig ::
    (Monoid m) =>
    (String -> m) ->
    (a -> m) ->
    TypeSig a -> m
renderTypeSig showString showCon = go
  where
    go sig0 = case sig0 of
        Con a -> showCon a
        Parens sig -> paren (go sig)
        Tupled sigs -> tupled (map go sigs)
        Apply sigs -> joinWith " " (map go sigs)
        Fun sigs -> joinWith " -> " (map go sigs)
        Var v -> showVar v
        Forall vs sig ->
            showString "forall " <>
            joinWith " " (map showVar vs) <>
            showString ". " <>
            go sig
        Context sigs sig ->
            tupled (map pred sigs) <>
            showString " => " <>
            go sig

    --joinWith :: String -> [m] -> m
    joinWith s = mconcat . intersperse (showString s)

    --paren :: m -> m
    paren m = showString "(" <> m <> showString ")"

    --tupled :: [m] -> m
    tupled = paren . joinWith ", "

    --pred :: Pred (TypeSig String) -> m
    pred p = case p of
        ClassPred sigs -> joinWith " " (map go sigs)
        EqPred _ a b -> go a <> showString " ~ " <> go b

    --showVar :: TypeVar -> m
    showVar (TypeVar s _) = showString s


typeSigShowS :: TypeSig String -> ShowS
typeSigShowS = appEndo . renderTypeSig (Endo . showString) (Endo . showString)


typeSigString :: (a -> String) -> TypeSig a -> String
typeSigString f sig = typeSigShowS (fmap f sig) ""


renderTypeUnqual :: Type -> String
renderTypeUnqual = typeSigString rawName . typeSig

typeSig :: Type -> TypeSig TypeConLink
typeSig = snd . typeSigPrec


data Prec = TopPrec | FunPrec | AppPrec | ConPrec
    deriving (Show, Eq, Ord)

typeSigPrec :: Type -> (Prec, TypeSig TypeConLink)
typeSigPrec = cata typeSigAlg

typeSigAlg :: TypeF (Prec, TypeSig TypeConLink)
                    -> (Prec, TypeSig TypeConLink)
typeSigAlg t0 = case t0 of
    VarF v -> (ConPrec, Var v)
    ConF a -> (ConPrec, Con a)
    ApplyF c a ->
        (,) AppPrec $ Apply [prec TopPrec c, prec AppPrec a]
    FunF a b ->
        (,) FunPrec $ Fun [prec FunPrec a, prec TopPrec b]
    ForallF vs (_, sig) ->
        (,) TopPrec $ Forall vs sig
    ContextF ps (_, sig) ->
        (,) TopPrec $ Context (map (fmap typeSig) ps) sig
  where
    prec :: Prec -> (Prec, TypeSig TypeConLink) -> TypeSig TypeConLink
    prec prec0 (prec, sig)
         | prec <= prec0 = Parens sig
         | otherwise = sig

