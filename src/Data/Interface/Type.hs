module Data.Interface.Type
(
    module Data.Interface.Type.Type
  , module Data.Interface.Type.Env
  , module Data.Interface.Type.Pretty
)
where

import Data.Interface.Name

import Data.Interface.Type.Type
import Data.Interface.Type.Env
import Data.Interface.Type.Pretty


-- | A single step of a Type traversal
data TypeStep
    = StepApply StepDir
    | StepFun StepDir
    | StepForall
    deriving (Show, Eq, Ord)

data StepDir = StepLeft | StepRight
    deriving (Show, Eq, Ord)


-- | Traverse two Types in parallel, producing a list of their differences.
typeDifferences :: Type -> Type -> [([TypeStep], Type, Type)]
typeDifferences = go []
  where
    go trail a b = case (a,b) of
        (Apply a0 b0, Apply a1 b1) ->
            go (StepApply StepLeft : trail) a0 a1 ++
            go (StepApply StepRight : trail) b0 b1
        (Fun a0 b0, Fun a1 b1) ->
            go (StepFun StepLeft : trail) a0 a1 ++
            go (StepFun StepRight :trail) b0 b1
        (Forall vs0 t0, Forall vs1 t1)
            | vs0 == vs1 ->  -- TODO
                go (StepForall : trail) t0 t1
        _ | a == b -> []
          | otherwise -> [(trail, a, b)]
  -- TODO: deal with alpha equivalence


    

wiredTypeKind :: WiredType -> Kind
wiredTypeKind w = case w of
    WBool     -> basicTypeConKind 0
    WEq       -> FunKind StarKind ConstraintKind
    WOrdering -> FunKind StarKind ConstraintKind
    WChar     -> basicTypeConKind 0
    WDouble   -> basicTypeConKind 0
    WFloat    -> basicTypeConKind 0
    WInt      -> basicTypeConKind 0
    WWord     -> basicTypeConKind 0
    WList     -> basicTypeConKind 1
    WUnit     -> basicTypeConKind 0
    WTuple a  -> basicTypeConKind a
