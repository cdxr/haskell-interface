{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Interface.Type.Render
(
    TypeRender(..)
  , renderType
  , renderTypeDiff'
  , stringTypeRender
  , renderTypeString
) where

import Data.Monoid
import Data.List ( intersperse )
import Data.Functor.Foldable ( cata, embed )

import Data.Interface.Change
import Data.Interface.Change.View

import Data.Interface.Name ( rawName )
import Data.Interface.Type ( Type, TypeF(..), TypeConLink, TypeVar(..),
                             Pred(..), TypeApply(..), varName, stripForall )
import Data.Interface.Type.Diff


-- | A @TypeRender m@ for a `Monoid` @m@ provides a means of rendering a type
-- signature. @m@ will typically represent a display format such as text or
-- HTML.
data TypeRender m = TypeRender
    { renderCon    :: TypeConLink -> m
    , renderVar    :: TypeVar -> m
    , renderString :: String -> m
    } deriving (Functor)

-- | A simple `TypeRender String` that does not qualify type constructors
stringTypeRender :: TypeRender String
stringTypeRender = TypeRender
    { renderCon = rawName
    , renderVar = varName
    , renderString = id
    }

typeRenderShowS :: (TypeConLink -> String) -> TypeRender (Endo String)
typeRenderShowS showCon =
    Endo . showString <$> stringTypeRender {renderCon = showCon}

-- | Render a type signature as a `String`.
--
-- @renderTypeString `rawName`@ produces an unqualified signature.
--
-- @renderTypeString `qualName`@ produces a fully-qualified signature.
renderTypeString :: (TypeConLink -> String) -> Type -> String
renderTypeString showCon t =
    renderType (typeRenderShowS showCon) t `appEndo` ""


renderType :: (Monoid m) => TypeRender m -> Type -> m
renderType tr = snd . cata (renderTypeAlg tr) . stripForall

data Prec = TopPrec | FunPrec | AppPrec | ConPrec
    deriving (Show, Eq, Ord)

instance Monoid Prec where
    mempty = TopPrec
    mappend = min


typeTermPrec :: TypeF a -> Prec
typeTermPrec t = case t of
    VarF{}     -> ConPrec
    ConF{}     -> ConPrec
    ApplyF a   -> case a of
        ConList{}  -> ConPrec
        ConTuple{} -> ConPrec
        _          -> AppPrec
    FunF{}     -> FunPrec
    ForallF{}  -> TopPrec
    ContextF{} -> TopPrec


renderTypeApply :: (Monoid m) => TypeRender m -> TypeApply (Prec, m) -> m
renderTypeApply tr a = case a of
    ConApply t ts ->
        joinWith tr " " $ prec tr TopPrec t : map (prec tr AppPrec) ts
    ConList t ->
        renderString tr "[" <> prec tr TopPrec t <> renderString tr "]"
    ConTuple _arity ts -> tupled tr $ map (prec tr TopPrec) ts

renderTypeAlg ::
    forall m. (Monoid m) =>
    TypeRender m -> TypeF (Prec, m) -> (Prec, m)
renderTypeAlg tr@TypeRender{..} term =
    (,) (typeTermPrec term) $ case term of
        VarF v -> renderVar v
        ConF c -> renderCon c
        ApplyF a -> renderTypeApply tr a
            --prec TopPrec c <> renderString " " <> prec AppPrec a
        FunF a b ->
            prec tr FunPrec a <> renderString " -> " <> prec tr TopPrec b
        ForallF vs (_, m) ->
            renderString "forall " <> joinWith tr " " (map renderVar vs) <>
            renderString ". " <> m
        ContextF ps (_, m) ->
            tupled tr (map (renderPred tr) ps) <> renderString " => " <> m


prec :: (Monoid m) => TypeRender m -> Prec -> (Prec, m) -> m
prec tr prec0 (prec, sig)
     | prec <= prec0 = paren tr sig
     | otherwise = sig

paren :: (Monoid m) => TypeRender m -> m -> m
paren tr m = renderString tr "(" <> m <> renderString tr ")"

tupled :: (Monoid m) => TypeRender m -> [m] -> m
tupled tr = paren tr . joinWith tr ", "


joinWith :: (Monoid m) => TypeRender m -> String -> [m] -> m
joinWith rt s = mconcat . intersperse (renderString rt s)


renderPred :: (Monoid m) => TypeRender m -> Pred Type -> m
renderPred tr p = case p of
    ClassPred ts -> joinWith tr " " $ map (renderType tr) ts
    EqPred _ a b ->
        renderType tr a <> renderString tr " ~ " <> renderType tr b


renderTypeDiff' ::
    (Monoid m) =>
    RenderCombined m ->             -- ^ how to display differences
    TypeRender m ->                 -- ^ how to render type information
    TypeDiff' -> DiffView m
renderTypeDiff' dr tr =
    fmap snd . cata (renderTypeDiff'Alg dr tr) . stripForallDiff'


renderTypeDiffAlg ::
    forall m. (Monoid m) =>
    RenderCombined m ->
    TypeRender m ->
    DiffTypeF Type (DiffView (Prec, m)) -> DiffView (Prec, m)
renderTypeDiffAlg rc tr td0 = case td0 of
    NoDiffTypeF t -> pure $ renderTerm t
    ReplaceTypeF t0 t1 ->
        elemViewPrec rc $ Elem $ Change (renderTerm t0) (renderTerm t1)
    SameTypeF c -> fmap (renderTypeAlg tr) $ sequence c
  where
    renderTerm :: TypeF Type -> (Prec, m)
    renderTerm = cata (renderTypeAlg tr) . embed

elemViewPrec ::
    (Monoid m) =>
    RenderCombined m ->
    Elem (Change (Prec, m)) (Prec, m) ->
    DiffView (Prec, m)
elemViewPrec rc = elemView' (combineRC renderCombinedPrec rc)
  where
    renderCombinedPrec e = Just $ case e of
        Removed p -> p
        Added p -> p
        Elem (Replace a b) -> min a b


renderTypeDiff'Alg ::
    forall m. (Monoid m) =>
    RenderCombined m ->
    TypeRender m ->
    TypeDiff'F (DiffView (Prec, m)) -> DiffView (Prec, m)
renderTypeDiff'Alg rc tr td0 = case td0 of
    ChangeContext cc c ->
        (,) TopPrec <$> (mappend <$> context <*> fmap snd c)
          where
            cxt = renderContext . map (pure . renderPred tr)
            context = case cc of
                RemovedContext ps -> elemView' rc . Removed =<< cxt ps
                AddedContext ps   -> elemView' rc . Added =<< cxt ps
                ContextElems p ->
                    renderContext $ map renderElemPred (patienceElems p)
    {-
    NoDiffApply t0 ts -> case (t0, ts) of
        (Type.Con (Qual _ "[]"), [a]) ->
            renderString tr "[" ++ renderType tr a ++ renderString tr "]"
        _ -> pure (AppPrec, renderType tr $ apply t0 ts)
    -}
    TypeDiff'F dtf -> renderTypeDiffAlg rc tr dtf
  where
    renderElemPred :: Elem (Pred Type) (Pred Type) -> DiffView m
    renderElemPred e = case e of
        Removed p -> elemView' rc $ Removed $ renderPred tr p
        Added p -> elemView' rc $ Added $ renderPred tr p
        Elem p -> pure $ renderPred tr p

    renderContext :: [DiffView m] -> DiffView m
    renderContext ps =
        pure (renderString tr "(") <>
        (joinWith tr ", " <$> sequence ps) <>
        pure (renderString tr ") => ")
