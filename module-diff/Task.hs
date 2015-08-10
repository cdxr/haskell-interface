{-# LANGUAGE DeriveFunctor #-}

module Task where

import Data.Char ( isLower )

import qualified System.FilePath as Path

import LoadPackageInterface ( PackageSelector(..), readPackageSelector )

import Data.Interface ( PackageInterface, PackageDiff,
                        ModuleInterface, ModuleDiff,
                        ModuleName, isValidModuleName )

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
    c:_ | isLower c && notElem '/' s -> Just $ readPackageSelector <$> t
    _ -> Nothing


-- | The filepath to a Module, or the name of an installed Module
type ModuleTarget = Target (Either FilePath ModuleName)

-- | The `String` used to identify a `ModuleTarget` to the user.
moduleTargetString :: ModuleTarget -> String
moduleTargetString (Target _ e) = either id id e

resolveModuleTarget :: Target String -> Maybe ModuleTarget
resolveModuleTarget t@(Target _ s)
    | isValidModuleName s = Just $ fmap Right t
    | Path.isValid s && validExtension s = Just $ fmap Left t
    | otherwise = Nothing
  where
    validExtension fp = any (== Path.takeExtension fp) [".hs", ".lhs"]


resolveTarget :: Target String -> Program (Either PackageTarget ModuleTarget)
resolveTarget t@(Target _ s)
    | Just pt <- resolvePackageTarget t =
        pure $ Left pt
    | Just mt <- resolveModuleTarget t =
        pure $ Right mt
    | otherwise =
        error $ "target does not represent a package or module: " ++ s


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
        ShowCommand t -> either PrintPackage PrintModule <$> resolveTarget t
        CompareCommand c -> case c of
            CompareThisInstalled ->
                error "compare command without arguments unimplemented" -- TODO
            CompareInstalled t ->
                error "compare command with single argument unimplemented"
            CompareThese t0 t1 -> do
                et0 <- resolveTarget t0
                et1 <- resolveTarget t1

                case (et0, et1) of
                    (Left pt0, Left pt1) ->
                        pure $ ComparePackages pt0 pt1
                    (Right mt0, Right mt1) ->
                        pure $ CompareModules mt0 mt1
                    _ -> let showTargetType Left{}  = "package"
                             showTargetType Right{} = "module"
                         in error $ unwords
                             [ "incompatible targets: can't compare"
                             , showTargetType et0
                             , "to"
                             , showTargetType et1
                             ]



-- | The "payload" of the program, containing the result for the user-given
-- task. This information is presented to the user according to the chosen
-- `OutputFormat`.
data ProgramResult
    = APackage PackageInterface
    | APackageDiff PackageDiff
    | AModule ModuleTarget ModuleInterface
    | AModuleDiff ModuleTarget ModuleTarget ModuleDiff
