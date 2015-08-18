{-# LANGUAGE PatternSynonyms #-}

{-  This module seeks to provide every definition from in Test.hs with
 -  alterations.
-}

module TestChangeAll
(
-- * Local exports
    FooTypeCon(..)              -- data constructor changed type
  , foo                         -- value changed type
  , pattern Foo                 -- pattern changed type

  , NewTypeCon(..)              -- new type
  , newValue                    -- new value

  , BarTypeCon                  -- changed kind

  , makeList                    -- added constraint to type of value

  -- TODO:
  --    alterations to type classes and type families from Test.hs
)
where


newtype FooTypeCon = FooDataCon Double      -- change `Int` to `Double`
    deriving (Show, Eq)                     -- add `Eq` instance

foo :: Double -> FooTypeCon                 -- change type from `FooTypCon`
foo = FooDataCon                            --  to `Double -> FooTypeCon`

pattern Foo a = FooDataCon a                -- no written difference,
                                            -- change type
                                            --  from `Int -> FooDataCon`
                                            --  to `Double -> FooDataCon`


data NewTypeCon a b = NewDataCon a b
    deriving (Show)

newValue :: a -> b -> b -> NewTypeCon a b
newValue a b _ = NewDataCon a b


data BarTypeCon p = BarDataCon              -- change kind from `*` to `* -> *`


makeList :: (Ord a) => [a]                  -- add `Ord` constraint
makeList = []


class LocalClass a where
    localClassValue1 :: a                    -- rename class method 

instance LocalClass FooTypeCon where
    localClassValue1 = foo 0

instance LocalClass Int where
    localClassValue1 = 0
