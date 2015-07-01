{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Interface.Change where

import Data.Bifunctor

import Data.Functor.Classes

import Data.Map ( Map )
import qualified Data.Map as Map


-- | @Change a@ represents an "old" value of @a@ paired with a corresponding
-- "new" value of @a@. There is a distinguished constructor for when these
-- values are considered equal, which depends on the context in which the
-- `Change` is used.
data Change a = NoChange a | Change a a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


change :: (a -> b) -> (a -> a -> b) -> Change a -> b
change n _ (NoChange a) = n a
change _ c (Change a b) = c a b


-- | @old c@ is the older value stored in @c@.
--
-- @
-- old (diff a b) = a
-- @
old :: Change a -> a
old = change id $ \a _ -> a

-- | @new c@ is the newer value stored in @c@.
--
-- @
-- new (diff a b) = b
-- @
new :: Change a -> a
new = change id $ \_ b -> b


instance Applicative Change where
    pure = NoChange

    NoChange f <*> c = f <$> c
    Change f0 f1 <*> c = Change (f0 $ old c) (f1 $ new c)

instance Monad Change where
    return = NoChange

    NoChange x >>= f = f x
    Change x y >>= f = Change (old $ f x) (new $ f y)

instance (Monoid a) => Monoid (Change a) where
    mempty = NoChange mempty
    mappend a b = mappend <$> a <*> b


-- | @Replace a@ is like @Change a@, except that the values are always
-- considered different. It should appear in contexts where the values have
-- been statically determined to be different.
data Replace a = Replace a a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Replace where
    pure a = Replace a a
    Replace f g <*> Replace x y = Replace (f x) (g y)

instance Monad Replace where
    return a = Replace a a
    Replace x0 y0 >>= f = Replace x y
      where
        Replace x _ = f x0
        Replace _ y = f y0


-- | @Same a@ is a constant @a@. It is used in contexts expecting a @Change a@
-- when the value is statically guaranteed to be unchanged.
newtype Same a = Same a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

getSame :: Same a -> a
getSame (Same a) = a
{-# INLINABLE getSame #-}


-- | Class for any type @c@ that represents a pair of values of type @a@,
-- where one value is considered an updated or more recent version of the
-- other.
--
-- @
-- noDiff a = diff a a
-- @
--
-- @
-- toChange (noDiff a) = NoChange a
-- @
--
-- @
-- toChange (diff a b) = Change c d  ==>  (a,b) = (c,d)
-- @
--
-- @isChanged = isChanged . toChange@
--
class Diff a c | c -> a where
    toChange :: c -> Change a
    diff :: a -> a -> c

    noDiff :: a -> c
    noDiff a = diff a a
    {-# INLINABLE noDiff #-}

    isChanged :: c -> Bool
    isChanged c = case toChange c of
        NoChange{} -> False
        Change{}   -> True

    {-# MINIMAL diff, toChange #-}


fromChange :: (Diff a c) => Change a -> c
fromChange = change noDiff diff
{-# INLINABLE fromChange #-}

-- | @toReplace c@ is the @Replace@ containing the pair of values in @c@, which
-- has forgotten whether the values are the same.
toReplace :: (Diff a c) => c -> Replace a
toReplace = change pure Replace . toChange
{-# INLINABLE toReplace #-}

isSame :: (Diff a c) => c -> Bool
isSame = not . isChanged
{-# INLINABLE isSame #-}


instance (Eq a) => Diff a (Change a) where
    diff a b
        | a /= b    = Change a b
        | otherwise = NoChange b

    noDiff = NoChange

    toChange = id
    {-# INLINABLE toChange #-}


-- | trivial: considers all values to be distinct
instance Diff a (Replace a) where
    diff = Replace
    noDiff a = Replace a a

    toChange (Replace a b) = Change a b


-- | When @c@ represents a change to a value of type @c@, @Elem c a@ is 
-- a change, a newly-added @a@, or a removed @a@.
data Elem c a
    = Removed a   -- ^ the old value
    | Added a     -- ^ the new value
    | Elem c      -- ^ the old and new values
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


-- | Analagous to @fromMaybe@. @fromElem x@ is a function that converts an
-- @Elem c a@ to a @c@ by filling in @x@ for any missing value of type @a@.
fromElem :: (Diff a c) => a -> Elem c a -> c
fromElem x e = case e of
    Removed a -> diff a x
    Added b   -> diff x b
    Elem c    -> c

isElemChanged :: (Diff a c) => Elem c a -> Bool
isElemChanged e = case e of
    Removed{} -> True
    Added{}   -> True
    Elem c    -> isChanged c


toElemChange :: (Diff a c) => Elem c a -> Elem (Change a) a
toElemChange = first toChange


applyChange :: Change (a -> b) -> Elem (Change a) a -> Elem (Change b) b
applyChange c e = case e of
    Removed a -> Removed $ old c a
    Added b   -> Added $ new c b
    Elem cx   -> Elem $ c <*> cx


-- | @SetElem a@ represents an element of type @a@ that may have been added
-- or removed, but could not have changed.
type SetElem a = Elem (Same a) a

extractSetElem :: SetElem a -> a
extractSetElem e = case e of
    Removed a     -> a
    Added b       -> b
    Elem (Same a) -> a

mapSetElem :: (a -> b) -> SetElem a -> SetElem b
mapSetElem f = bimap (fmap f) f

setElemChange :: SetElem a -> Elem (Change a) a
setElemChange = first (NoChange . getSame)



-- | An @Elem (Change a) a@.
-- This is named for the class instance @Diff a (Change a)@, which uses the
-- notion of equality provided by `Eq`.
type ElemEq a = Elem (Change a) a

instance Bifunctor Elem where
    bimap f g e = case e of
        Removed a -> Removed (g a)
        Added a   -> Added (g a)
        Elem c    -> Elem (f c)


diffMaybe :: (Diff a c) => Maybe a -> Maybe a -> Maybe (Elem c a)
diffMaybe Nothing Nothing = Nothing
diffMaybe (Just a) Nothing = Just $ Removed a
diffMaybe Nothing (Just b) = Just $ Added b
diffMaybe (Just a) (Just b) = Just $ Elem (diff a b)


-- * Computing Diffs for structures

-- ** Set

{- TODO:
newtype SetDiff a = SetDiff { Map a (SetElem ()) }

instance Diff (Set a) (SetDiff a) where
-}


{-
type DiffSet c a = [Elem c a]
type DiffSetEq a = [ElemEq a]

-- | The differences in element inclusion between two sets
diffSet :: (Ord a) => Set a -> Set a -> DiffSetEq a
diffSet xs0 ys0 = go (Set.toAscList xs0) (Set.toAscList ys0)
  where
    go [] ys = map Added ys
    go xs [] = map Removed xs
    go (x:xs) (y:ys) = case compare x y of
        EQ -> Changed (NoChange y) : go xs ys
        LT -> Removed x : go xs (y:ys)
        GT -> Added y : go (x:xs) ys
-}


-- ** Map

-- | When @f a@ is a container of @a@s and @c@ is a change to an @a@,
-- @ElemChanges f c a@ is a change to container of @a@s in which 

-- | When @c@ represents a change to an @a@ and @f a@ is an associative
-- container of @a@s, @ElemChanges f c a@ represents a change to such a
-- container in which elements may have been added, removed, or changed.
data ElemChanges f c a
    = NoElemChanges (f a)           -- cached unchanged structure
    | ElemChanges (f (Elem c a))

instance (Show1 f, Show c, Show a) => Show (ElemChanges f c a) where
    showsPrec p ec = showParen (p > 10) $ case ec of
        NoElemChanges fa ->
            showString "NoElemChanges " . showsPrec1 11 fa
        ElemChanges fe ->
            showString "ElemChanges " . showsPrec1 11 fe


transElemChanges ::
    (forall x. f x -> g x) -> ElemChanges f c a -> ElemChanges g c a
transElemChanges nat ec = case ec of
    NoElemChanges fa -> NoElemChanges (nat fa)
    ElemChanges fe   -> ElemChanges (nat fe)

type MapDiff k = ElemChanges (Map k)
type MapDiffEq k a = MapDiff k (Change a) a

instance (Ord k, Diff a c) => Diff (Map k a) (MapDiff k c a) where
    diff = diffMap
    noDiff = NoElemChanges

    toChange d = case d of
        NoElemChanges m -> NoChange m
        ElemChanges m -> Map.foldMapWithKey f m
          where
            f k e = case e of
                Removed a -> Change (Map.singleton k a) mempty
                Added b   -> Change mempty (Map.singleton k b)
                Elem c    -> fmap (Map.singleton k) (toChange c)


viewMapDiff :: (Diff a c) => MapDiff k c a -> Map k (Elem c a)
viewMapDiff d = case d of
    NoElemChanges m -> Map.map (Elem . noDiff) m
    ElemChanges m -> m

diffMap :: (Ord k, Diff a c) => Map k a -> Map k a -> MapDiff k c a
diffMap a b = toMapDiff $ Map.mergeWithKey combine only1 only2 a b
  where
    combine _ x y = Just $ Elem (diff x y)
    only1 = Map.map Removed
    only2 = Map.map Added
    toMapDiff m
        | any isElemChanged m = ElemChanges m
        | otherwise = NoElemChanges b



{-
diffSetSummary :: (Diff a c) => DiffSet c a -> DiffSummary c a
diffSetSummary = transDiffSummary reverse . foldr (addDiffElem (:)) mempty

diffMapSummary' ::
    (Diff a c, Ord k) =>
    MapDiff k c a -> DiffSummary' (Map k) c a
diffMapSummary' m = case m of
    NoElemChanges m -> mempty { unchanged = m }
    ElemChanges m -> Map.foldrWithKey f mempty m
  where
    f :: (Diff a c, Ord k) =>
         k ->
         Elem c a ->
         DiffSummary' (Map k) c a -> DiffSummary' (Map k) c a
    f k d = addDiffElem (Map.insert k) d

diffMapSummary :: (Diff a c, Ord k) => MapDiff k c a -> DiffSummary c a
diffMapSummary = transDiffSummary Map.elems . diffMapSummary'


-- * DiffSummary

data DiffSummary' f c a = DiffSummary
    { unchanged :: f a
    , added     :: f a
    , removed   :: f a
    , changed   :: f c
    } deriving (Show, Eq, Ord)

type DiffSummary = DiffSummary' []


instance (Monoid (f a), Monoid (f c)) => Monoid (DiffSummary' f c a) where
    mempty = DiffSummary mempty mempty mempty mempty
    mappend a b = DiffSummary
        { unchanged = on (<>) unchanged a b
        , added     = on (<>) added a b
        , removed   = on (<>) removed a b
        , changed   = on (<>) changed a b
        } 

transDiffSummary ::
    (forall b. f b -> g b) ->
    DiffSummary' f c a -> DiffSummary' g c a
transDiffSummary f (DiffSummary a b c d) = DiffSummary (f a) (f b) (f c) (f d)

addDiff ::
    (Diff a c) =>
    (forall b. b -> f b -> f b) ->
    c -> DiffSummary' f c a -> DiffSummary' f c a
addDiff add c ds = case same c of
    Just a  -> ds { unchanged = add a (unchanged ds) }
    Nothing -> ds { changed   = add c (changed ds) }

addDiffElem ::
    (Diff a c) =>
    (forall b. b -> f b -> f b) ->
    Elem c a -> DiffSummary' f c a -> DiffSummary' f c a
addDiffElem add e ds = case e of
    Removed a -> ds { removed   = add a (removed ds) }
    Added a   -> ds { added     = add a (added ds)}
    Changed c -> addDiff add c ds
-}
