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
    FooTypeCon(..)  -- type with exported constructors
  , foo             -- value of local type

  , BarTypeCon      -- type with hidden constructors

  , makeList        -- value of imported type

-- * Re-exports
  , Bool                    -- re-export type
  , bool                    -- re-export value
  , module System.Timeout   -- re-export whole module (with single export)

  -- originates in Data.Foldable, imported from Data.List:
  , foldr

-- Local class instances:
--   instance Show Foo          -- instance for imported class

-- TODO:
--  class definition
--  pattern synonym
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


data BarTypeCon = BarDataCon


makeList :: [a]
makeList = []
