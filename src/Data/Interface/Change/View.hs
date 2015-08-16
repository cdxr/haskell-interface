{-# LANGUAGE DeriveFunctor #-}

module Data.Interface.Change.View where

import Data.Bifunctor ( bimap )
import Data.Interface.Change


-- | A @DiffView m@ for a `Monoid` @m@ is a pair of @m@s that represent the
-- two @a@s in a particular @Change a@, plus a possible @m@ that represents
-- a combined view of those @a@s.
data DiffView m = DiffView
    { viewCombined :: Maybe m
    , viewSeparate :: Change m
    } deriving (Show, Eq, Ord, Functor)

instance Applicative DiffView where
    pure m = DiffView (Just m) (NoChange m)
    DiffView fc fs <*> DiffView xc xs = DiffView (fc <*> xc) (fs <*> xs)

instance Monad DiffView where
    return = pure
    DiffView c s >>= f =
        DiffView { viewCombined = viewCombined . f =<< c
                 , viewSeparate = viewSeparate . f =<< s
                 }

instance (Monoid m) => Monoid (DiffView m) where
    mempty = pure mempty
    DiffView ac as `mappend` DiffView bc bs =
        DiffView (mappend <$> ac <*> bc) (mappend as bs)

changeView :: Change m -> DiffView m
changeView c = case c of
    NoChange m -> DiffView (Just m) c
    Change{}   -> DiffView Nothing c

elemView :: (Monoid m) => Elem (Change m) m -> DiffView m
elemView e = case e of
    Removed m -> DiffView Nothing $ Change m mempty
    Added m   -> DiffView Nothing $ Change mempty m
    Elem c -> changeView c


type RenderCombined m = Elem (Replace m) m -> Maybe m

elemView' :: (Monoid m) => RenderCombined m -> Elem (Change m) m -> DiffView m
elemView' rc e = case e of
    Elem NoChange{} -> elemView e
    _ -> (elemView e) { viewCombined = rc $ mapElem toReplace e }

combineRC :: RenderCombined m -> RenderCombined n -> RenderCombined (m,n)
combineRC rc0 rc1 e =
    (,) <$> rc0 (bimap (fmap fst) fst e) <*> rc1 (bimap (fmap snd) snd e)

