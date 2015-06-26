module Data.Interface.Type
(
    splitFunction
  , isFunction

  , module Data.Interface.Type.Type
  , module Data.Interface.Type.Env
)
where

import Data.Interface.Name

import Data.Interface.Type.Type
import Data.Interface.Type.Env


splitFunction :: Type -> Maybe (Type, Type)
splitFunction t0 = case t0 of
    Fun a b -> Just (a,b)
    _       -> Nothing

isFunction :: Type -> Bool
isFunction t0
    | Just _ <- splitFunction t0 = True
    | otherwise = False
