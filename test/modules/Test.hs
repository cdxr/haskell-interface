{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-  This module is test input for working with the ModuleInterface type.
 -  The goal is to eventually provide a small simple example of every sort
 -  of component found in an interface.
 -
 -  It has no dependencies except base, and is intended to be passed directly
 -  to GHC.
-}

module Test
(
-- * Local exports
    FooTypeCon(..)          -- type with exported constructors
  , foo                     -- value of local type
  , pattern Foo             -- pattern synonym

  , BarTypeCon              -- type with hidden constructors

  , makeList                -- value of imported type

  , LocalClass(..)          -- locally-defined typeclass

  , LocalTypeFamily         -- locally-defined type family

  , PromotedType(..)
  , HasPromoted(..)
  , hasPromoted

-- * Re-exports
  , Bool                    -- re-export type
  , bool                    -- re-export value
  , module System.Timeout   -- re-export whole module (with single export)

  , Eq(..)                  -- re-export typeclass

  -- originates in Data.Foldable, imported from Data.List:
  , foldr

-- Exported class instances:
--   instance Show FooTypeCon          -- imported class, local type
--   instance LocalClass FooTypeCon    -- local class, local type
--   instance LocalClass Int           -- local class, imported type
--

{- TODO:
    - typeclass with multiple parameters
-}
)
where

-- Imports for testing re-exports:
import Data.Bool ( bool )
import Data.List ( foldr )
import System.Timeout  -- chosen because it has only one export


newtype FooTypeCon = FooDataCon Int
    deriving (Show)

foo :: FooTypeCon
foo = FooDataCon 0

pattern Foo a = FooDataCon a


data BarTypeCon = BarDataCon


makeList :: [a]
makeList = []


class LocalClass a where
    localClassValue :: a

instance LocalClass FooTypeCon where
    localClassValue = foo

instance LocalClass Int where
    localClassValue = 0


type family LocalTypeFamily a :: *
type instance LocalTypeFamily FooTypeCon = Bool
type instance LocalTypeFamily Bool = Ordering


data PromotedType = PromotedData

data HasPromoted (pt :: PromotedType) = HasPromoted

hasPromoted :: HasPromoted 'PromotedData
hasPromoted = HasPromoted
