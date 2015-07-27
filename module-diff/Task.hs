{-# LANGUAGE DeriveFunctor #-}

module Task where

import Data.Bool ( bool )
import Data.Char ( isLower, isUpper )

import LoadPackageInterface ( PackageSelector(..), readPackageSelector,
                              PackageDB(..) )

import Data.Interface ( PackageInterface, PackageDiff,
                        ModuleInterface, ModuleDiff )

import Program
import ProgramArgs


-- | A `PackageName`, `PackageId`, or `InstalledPackageId`, optionally
-- annotated with a package database location.
type PackageTarget = Target PackageSelector

-- | Interpret a target as a `PackageTarget`. Evaluates to `Nothing` if the
-- `String` is not a valid `PackageSelector` or if the package database
-- location is not valid.
resolvePackageTarget :: Target String -> Maybe PackageTarget
resolvePackageTarget t@(Target _ s) = case s of
    c:_ | isLower c -> Just $ readPackageSelector <$> t
    _ -> Nothing


-- | The filepath to a Module, or the name of an installed Module
type ModuleTarget = Target String

-- | The `String` used to identify a `ModuleTarget` to the user.
moduleTargetString :: ModuleTarget -> String
moduleTargetString (Target _ s) = s  -- TODO

resolveModuleTarget :: Target String -> Maybe ModuleTarget
resolveModuleTarget t@(Target _ s) = case s of
    c:cs | isUpper c -> Just t    -- if the first character is upper-case,
    _ -> Nothing                  -- assume that this is a module target


-- | The task that the program is asked to perform. This is constructed after
-- program arguments have been correctly parsed, but before verifying the
-- existence or validity of any `PackageTarget` or `ModuleTarget`.
data ProgramTarget
    = PrintPackage PackageTarget
    | ComparePackages PackageTarget PackageTarget
    | PrintModule ModuleTarget
    | CompareModules ModuleTarget ModuleTarget
    deriving (Show, Eq, Ord)

-- | Determine the `ProgramTarget` from the implicitly-given `ProgramArgs`.
resolveProgramTarget :: Program ProgramTarget
resolveProgramTarget = do
    com <- getArg programCommand
    case com of
        ShowCommand t
            | Just pt <- resolvePackageTarget t ->
                pure $ PrintPackage pt
            | Just mt <- resolveModuleTarget t ->
                pure $ PrintModule mt
            | otherwise ->
                error "Target does not represent a package or module"
        CompareCommand c -> case c of
            CompareThisInstalled ->
                error "compare command without arguments unimplemented" -- TODO
            CompareInstalled t ->
                error "compare command with single argument unimplemented"
            CompareThese t0 t1
                | Just t <-
                    ComparePackages <$> resolvePackageTarget t0
                                    <*> resolvePackageTarget t1
                    -> pure t
                | Just t <-
                    CompareModules <$> resolveModuleTarget t0
                                   <*> resolveModuleTarget t1
                    -> pure t
                | otherwise ->
                    error "incompatible targets"


-- | The "payload" of the program, containing the result for the user-given
-- task. This information is presented to the user according to the chosen
-- `OutputFormat`.
data ProgramResult
    = APackage PackageInterface
    | APackageDiff PackageDiff
    | AModule ModuleTarget ModuleInterface
    | AModuleDiff ModuleTarget ModuleTarget ModuleDiff
