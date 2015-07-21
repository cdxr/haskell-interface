{-# LANGUAGE DeriveFunctor #-}

module Task where

import LoadPackageInterface ( PackageFilter, PackageDB )

import Data.Interface ( PackageInterface, ModuleInterface )


-- | A program task, parameterized over the types of packages and modules.
-- The type parameters are determined by the program phase.
data Task p m
    = PrintPackage p
--  | ComparePackages p p
    | PrintModule m
    | CompareModules m m
--  | RunTestModule FilePath
    deriving (Show, Eq, Ord, Functor)


bitraverseTask ::
    (Applicative f) =>
    (a -> f p) ->
    (b -> f m) ->
    Task a b -> f (Task p m)
bitraverseTask p m t = case t of
    PrintPackage a -> PrintPackage <$> p a
    PrintModule b  -> PrintModule <$> m b
    CompareModules b0 b1 -> CompareModules <$> m b0 <*> m b1



type TargetTask = Task PackageTarget ModuleTarget

data PackageTarget = PackageTarget PackageFilter [PackageDB]
    deriving (Show, Eq, Ord)

-- | The filepath to a Module, or the name of an installed Module
type ModuleTarget = String



type LoadedTask = Task PackageInterface (ModuleTarget, ModuleInterface)
-- note: ^ original ModuleTarget stored for reporting purposes
