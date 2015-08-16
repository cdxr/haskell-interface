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
import Data.Foldable ( toList )

import Data.Functor.Classes

import Data.Map ( Map )
import qualified Data.Map as Map

import qualified Data.Algorithm.Patience as Patience


-- | @Change a@ represents an "old" value of @a@ paired with a corresponding
-- "new" value of @a@. There is a distinguished constructor for when these
-- values are considered equal, which depends on the context in which the
-- `Change` is used.
data Change a = NoChange a | Change a a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


-- | Case analysis for the @Change a@ type, analagous to `maybe` from Prelude.
-- @change f g c@ evaluates to @f x@ when @c@ is @NoChange x@, and evaluates
-- to @g x y@ when @c@ is @Change x y@.
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


-- | An instance @ToChange a c@ is for any type @c@ that represents a pair of
-- values of type @a@, in which one value is considered an updated or more
-- recent version of the other.
--
-- @isChanged = isChanged . toChange@
--
class ToChange a c | c -> a where
    toChange :: c -> Change a

    isChanged :: c -> Bool
    isChanged c = case toChange c of
        NoChange{} -> False
        Change{}   -> True

    {-# MINIMAL toChange #-}

-- | @toReplace c@ is the @Replace@ containing the pair of values in @c@, which
-- has forgotten whether the values are the same.
toReplace :: (ToChange a c) => c -> Replace a
toReplace c = case toChange c of
    NoChange a -> Replace a a
    Change a b -> Replace a b
{-# INLINABLE toReplace #-}

isNotChanged :: (ToChange a c) => c -> Bool
isNotChanged = not . isChanged
{-# INLINABLE isNotChanged #-}

getNoDiff :: (ToChange a c) => c -> Maybe a
getNoDiff c = case toChange c of
    NoChange a -> Just a
    _          -> Nothing



-- | An instance @Diff a c@ is for a type @c@ with an instance of
-- @ToChange a c@ that admits a canonical function `diff` for producing a
-- @c@ from two @a@s.
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
class (ToChange a c) => Diff a c | c -> a where
    diff :: a -> a -> c

    noDiff :: a -> c
    noDiff a = diff a a
    {-# INLINABLE noDiff #-}

    {-# MINIMAL diff #-}


fromChange :: (Diff a c) => Change a -> c
fromChange = change noDiff diff
{-# INLINABLE fromChange #-}

castDiff :: (ToChange a c, Diff a c') => c -> c'
castDiff = fromChange . toChange
{-# INLINABLE castDiff #-}



instance ToChange a (Change a) where
    toChange = id
    {-# INLINABLE toChange #-}

instance (Eq a) => Diff a (Change a) where
    diff a b
        | a /= b    = Change a b
        | otherwise = NoChange b

    noDiff = NoChange


instance ToChange a (Replace a) where
    toChange (Replace a b) = Change a b

-- | trivial: considers all values to be distinct
instance Diff a (Replace a) where
    diff = Replace
    noDiff a = Replace a a


-- | When @c@ represents a change to a value of type @c@, @Elem c a@ is 
-- a change, a newly-added @a@, or a removed @a@.
data Elem c a
    = Removed a   -- ^ the old value
    | Added a     -- ^ the new value
    | Elem c      -- ^ the old and new values
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


getRemoved :: Elem c a -> Maybe a
getRemoved e = case e of
    Removed a -> Just a
    _         -> Nothing

getAdded :: Elem c a -> Maybe a
getAdded e = case e of
    Added a -> Just a
    _         -> Nothing

getElem :: Elem c a -> Maybe c
getElem e = case e of
    Elem c -> Just c
    _      -> Nothing


-- | Analagous to @fromMaybe@. @fromElem x@ is a function that converts an
-- @Elem c a@ to a @c@ by filling in @x@ for any missing value of type @a@.
fromElem :: (Diff a c) => a -> Elem c a -> c
fromElem x e = case e of
    Removed a -> diff a x
    Added b   -> diff x b
    Elem c    -> c

isElemChanged :: (ToChange a c) => Elem c a -> Bool
isElemChanged e = case e of
    Removed{} -> True
    Added{}   -> True
    Elem c    -> isChanged c


castElem :: (ToChange a c, Diff a c') => Elem c a -> Elem c' a
castElem = first castDiff
{-# INLINABLE castElem #-}

mapElem :: (c -> c') -> Elem c a -> Elem c' a
mapElem = first

applyChange :: Change (a -> b) -> Elem (Change a) a -> Elem (Change b) b
applyChange c e = case e of
    Removed a -> Removed $ old c a
    Added b   -> Added $ new c b
    Elem cx   -> Elem $ c <*> cx


instance Bifunctor Elem where
    bimap f g e = case e of
        Removed a -> Removed (g a)
        Added a   -> Added (g a)
        Elem c    -> Elem (f c)



-- | @SetElem a@ represents an element of type @a@ that may have been added
-- or removed, but could not have changed.
type SetElem a = Elem (Same a) a

extractSetElem :: SetElem a -> a
extractSetElem e = case e of
    Removed a     -> a
    Added a       -> a
    Elem (Same a) -> a

mapSetElem :: (a -> b) -> SetElem a -> SetElem b
mapSetElem f = bimap (fmap f) f

setElemChange :: SetElem a -> Elem (Change a) a
setElemChange = first (NoChange . getSame)



-- | An @Elem (Change a) a@.
-- This is named for the class instance @Diff a (Change a)@, which uses the
-- notion of equality provided by `Eq`.
type ElemEq a = Elem (Change a) a

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

elemList :: (Diff a c, Foldable f) => ElemChanges f c a -> [Elem c a]
elemList ec = case ec of
    NoElemChanges as -> map (Elem . noDiff) (toList as)
    ElemChanges es   -> toList es


transElemChanges ::
    (forall x. f x -> g x) -> ElemChanges f c a -> ElemChanges g c a
transElemChanges nat ec = case ec of
    NoElemChanges fa -> NoElemChanges (nat fa)
    ElemChanges fe   -> ElemChanges (nat fe)

type MapDiff k = ElemChanges (Map k)
type MapDiffEq k a = MapDiff k (Change a) a

instance (Ord k, ToChange a c) => ToChange (Map k a) (MapDiff k c a) where
    toChange d = case d of
        NoElemChanges m -> NoChange m
        ElemChanges m -> Map.foldMapWithKey f m
          where
            f k e = case e of
                Removed a -> Change (Map.singleton k a) mempty
                Added b   -> Change mempty (Map.singleton k b)
                Elem c    -> fmap (Map.singleton k) (toChange c)

instance (Ord k, Diff a c) => Diff (Map k a) (MapDiff k c a) where
    diff = diffMap
    noDiff = NoElemChanges


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


-- * Patience diff algorithm

data Patience a = Patience { patienceElems' :: [Elem (Replace a) a] }
    deriving (Show, Eq, Ord)

patienceElems :: Patience a -> [Elem a a]
patienceElems = map (mapElem mostRecent) . patienceElems'
  where
    mostRecent (Replace _ a) = a

instance ToChange [a] (Patience a) where
    toChange (Patience es) = reverse <$> foldr go (Change [] []) es
      where
        go e (Change as bs) = case e of
            Removed a          -> Change (a:as)  bs
            Added b            -> Change as     (b:bs)
            Elem (Replace a b) -> Change (a:as) (b:bs)

instance (Ord a) => Diff [a] (Patience a) where
    diff as bs = Patience $ map toElem (Patience.diff as bs)
      where
        toElem item = case item of
            Patience.Old a -> Removed a
            Patience.New b -> Added b
            Patience.Both a b -> Elem (Replace a b)
