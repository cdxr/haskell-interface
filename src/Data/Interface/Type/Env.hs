module Data.Interface.Type.Env where

import Data.Maybe ( fromMaybe )
import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Interface.Name
import Data.Interface.Name.Map

import Data.Interface.Type.Type


newtype TypeEnv = TypeEnv
    { typeEnvMap :: Map ModuleName (NameMap TypeCon)
    } deriving (Show, Eq, Ord)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

lookupModule :: ModuleName -> TypeEnv -> NameMap TypeCon
lookupModule modName =
    fromMaybe emptyNameMap . Map.lookup modName . typeEnvMap

-- | Apply the given transformation to the corresponding module map.
-- If there is no map for that module, it is created.
updateModule ::
    ModuleName ->
    (NameMap TypeCon -> NameMap TypeCon) ->
    TypeEnv -> TypeEnv
updateModule modName f =
    TypeEnv . Map.alter (Just . f . fromMaybe emptyNameMap) modName . typeEnvMap


data MissingType
    = MissingModule ModuleName
    | MissingType (Qual TypeName)
    deriving (Show, Eq)

lookupType :: Qual TypeName -> TypeEnv -> Either MissingType TypeCon
lookupType q env =
    case Map.lookup modName $ typeEnvMap env of
        Nothing -> Left $ MissingModule modName
        Just tmap -> case lookupName q tmap of
            Nothing -> Left $ MissingType q
            Just t  -> Right $ unName t
  where
    modName = qualModuleName q


insertType :: Qual TypeCon -> TypeEnv -> TypeEnv
insertType (Qual modName typeCon) = updateModule modName $ insertHasName typeCon

