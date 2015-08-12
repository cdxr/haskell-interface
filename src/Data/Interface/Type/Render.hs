{-# LANGUAGE DeriveFunctor #-}

module Data.Interface.Type.Render
(
    renderTypeUnqual
  -- , renderTypeQual

  , TypeSig(..)
  , typeSig
  , renderTypeSig
  , renderTypeSigA
) where

import Data.Monoid
import Data.List ( intersperse )
import Data.Functor.Foldable ( cata, embed )
import Data.Bifunctor

import Data.Interface.Change
import Data.Interface.Name ( rawName )
import Data.Interface.Type ( Type, TypeF(..),
                             TypeConLink, TypeVar(..), Pred(..) )
import Data.Interface.Type.Diff


data TypeSig con a
    = Con con
    | Var TypeVar
    | Annotate a (TypeSig con a)
    | Parens (TypeSig con a)
    | Tupled [TypeSig con a]
    | Apply [TypeSig con a]
    | Fun (Maybe a) (TypeSig con a) (TypeSig con a)
    | Forall [TypeVar] (TypeSig con a)
    | Context (Maybe a)
              [(Pred (TypeSig con a), Maybe a)]
              (TypeSig con a)
    deriving (Functor)


--data PredSig =


mapCon :: (con -> con') -> TypeSig con a -> TypeSig con' a
mapCon f sig0 = case sig0 of
    Con con -> Con (f con)
    Var v -> Var v
    Annotate a sigs -> Annotate a (mapCon f sigs)
    Parens sig -> Parens (mapCon f sig)
    Tupled sigs -> Tupled (map (mapCon f) sigs)
    Apply sigs -> Apply (map (mapCon f) sigs)
    Fun ma arg res -> Fun ma (mapCon f arg) (mapCon f res)
    Forall vs sig -> Forall vs (mapCon f sig)
    Context ma ps sig ->
        Context ma ((map.first.fmap.mapCon) f ps) (mapCon f sig)


renderTypeSig ::
    (Monoid m) =>
    (String -> m) ->
    (con -> m) ->
    TypeSig con a -> m
renderTypeSig = renderTypeSigA (const id)


-- | Render a type signature that contains annotations
renderTypeSigA ::
    (Monoid m) =>
    (a -> m -> m) ->    -- ^ apply annotation to underlying rendered term
    (String -> m) ->    -- ^ render string literal
    (con -> m) ->       -- ^ render a type constructor
    TypeSig con a -> m
renderTypeSigA annot showString showCon = toplevel
  where
    toplevel sig0 = case sig0 of
        Forall vs sig -> go sig     -- hide toplevel forall
        _ -> go sig0

    go sig0 = case sig0 of
        Con a -> showCon a
        Var v -> showVar v
        Annotate a m -> annot a (go m)
        Parens sig -> paren (go sig)
        Tupled sigs -> tupled (map go sigs)
        Apply sigs -> joinWith " " (map go sigs)
        Fun ma arg res ->
            maybeAnnot ma (go arg <> showString " -> ")
                <> go res
        Forall vs sig ->
            showString "forall " <>
            joinWith " " (map showVar vs) <>
            showString ". " <>
            go sig
        Context ma sigs sig ->
            maybeAnnot ma (tupled (map pred sigs) <> showString " => ")
                <> go sig

    maybeAnnot = maybe id annot

    --joinWith :: String -> [m] -> m
    joinWith s = mconcat . intersperse (showString s)

    --paren :: m -> m
    paren m = showString "(" <> m <> showString ")"

    --tupled :: [m] -> m
    tupled = paren . joinWith ", "

    --pred :: Pred (TypeSig String) -> m
    pred (p, ma) = maybeAnnot ma $ case p of
        ClassPred sigs -> joinWith " " $ map go sigs
        EqPred _ a b -> go a <> showString " ~ " <> go b

    --showVar :: TypeVar -> m
    showVar (TypeVar s _k) = showString s


typeSigShowS :: TypeSig String a -> ShowS
typeSigShowS = appEndo . renderTypeSig endoShowS endoShowS
  where
    endoShowS :: String -> Endo String
    endoShowS = Endo . showString


typeSigString :: (con -> String) -> TypeSig con a -> String
typeSigString f sig = typeSigShowS (mapCon f sig) ""


renderTypeUnqual :: Type -> String
renderTypeUnqual = typeSigString rawName . typeSig

typeSig :: Type -> TypeSig TypeConLink a
typeSig = snd . typeSigPrec


data Prec = TopPrec | FunPrec | AppPrec | ConPrec
    deriving (Show, Eq, Ord)

typeSigPrec :: Type -> (Prec, TypeSig TypeConLink a)
typeSigPrec = cata typeSigAlg

typeSigAlg
    :: TypeF (Prec, TypeSig TypeConLink a) -> (Prec, TypeSig TypeConLink a)
typeSigAlg t0 = case t0 of
    VarF v -> (ConPrec, Var v)
    ConF a -> (ConPrec, Con a)
    ApplyF c a ->
        (,) AppPrec $ Apply [prec TopPrec c, prec AppPrec a]
    FunF a b ->
        (,) FunPrec $ Fun Nothing (prec FunPrec a) (prec TopPrec b)
    ForallF vs (_, sig) ->
        (,) TopPrec $ Forall vs sig
    ContextF ps (_, sig) ->
        (,) TopPrec $ Context Nothing
                              (map (\p -> (fmap typeSig p, Nothing)) ps)
                              sig
  where
    prec :: Prec -> (Prec, TypeSig TypeConLink a) -> TypeSig TypeConLink a
    prec prec0 (prec, sig)
         | prec <= prec0 = Parens sig
         | otherwise = sig

{-
data TypeDiffSig con
    = ChangeTypeSig (Change (TypeSig con ()))
    | TypeDiffSig (TypeSig con DiffAnnot)
    deriving (Show, Eq, Ord)
-}

type TypeDiffSig = TypeSig TypeConLink DiffAnnot

data DiffAnnot = AnnotRemoved | AnnotAdd | AnnotChanged
    deriving (Show, Eq, Ord)

typeDiffSig :: TypeDiff -> Change TypeDiffSig
typeDiffSig = fmap snd . cata typeDiffSigAlg

typeDiffSigAlg
    :: DiffTypeF Type (Change (Prec, TypeDiffSig))
    -> Change (Prec, TypeDiffSig)
typeDiffSigAlg dt0 = case dt0 of
    NoDiffTypeF t ->
        NoChange $ typeSigPrec (embed t)
    ReplaceTypeF t0 t1 ->
        fmap (second (Annotate AnnotChanged) . typeSigPrec . embed) $
            Change t0 t1
    SameTypeF t -> typeSigAlg <$> sequence t



typeDiff'Sig :: TypeDiff' -> Change TypeDiffSig
typeDiff'Sig = fmap snd . cata typeDiff'SigAlg

typeDiff'SigAlg
    :: DiffTypeF' Type (Change (Prec, TypeDiffSig))
    -> Change (Prec, TypeDiffSig)
typeDiff'SigAlg dt' = case dt' of
    BasicDiffTypeF dt -> typeDiffSigAlg dt
    ChangeContext ps0 ps1 c -> _ <$> Change ps0 ps1 <*> c 

-- | Given two versions of a Context
contextDiff
    :: [Pred Type] -> [Pred Type] -> Change (TypeDiffSig -> TypeDiffSig)
contextDiff ps0 ps1
    | ps0 == ps1 = Same (Context ps1)
    | otherwise = error "undefined: contextDiff"


{-
renderTypeDiffAlg :: (Render a) => DiffTypeF Type (Prec, a) -> (Prec, RDoc)
renderTypeDiffAlg td0 = case td0 of
    NoDiffTypeF t -> (ConPrec, doc $ embed t)
    ReplaceTypeF t0 t1 ->
        (,) ConPrec $ style braces $
            style red (doc $ embed t0) <+>
            text' "/" <+>
            style green (doc $ embed t1)
    SameTypeF fc -> renderTypeAlg $ fmap (second doc) fc
  where
    replaceDoc :: Replace Type -> RDoc
    replaceDoc (Replace t0 t1) =
        style braces $
            style red (doc t0) <+> text' "/" <+> style green (doc t1)
-}
