module Data.Interface.Type.Env where

import Data.Maybe ( fromMaybe )
import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Interface.Name

import Data.Interface.Type.Type


type TypeMap = Map RawName Type


newtype TypeEnv = TypeEnv
    { typeEnvMap :: Map ModuleName TypeMap
    } deriving (Show, Eq, Ord)

data MissingType
    = MissingModule ModuleName
    | MissingType (Qual TypeName)
    deriving (Show, Eq)

lookupModule :: ModuleName -> TypeEnv -> Maybe TypeMap
lookupModule modName = Map.lookup modName . typeEnvMap

-- | Apply the given transformation to the corresponding module map.
-- If there is no map for that module, it is created.
updateModule :: ModuleName -> (TypeMap -> TypeMap) ->  TypeEnv -> TypeEnv
updateModule modName f =
    TypeEnv . Map.alter (Just . f . fromMaybe Map.empty) modName . typeEnvMap


lookupType :: Qual TypeName -> TypeEnv -> Either MissingType Type
lookupType qual env =
    case lookupModule modName env of
        Nothing -> Left $ MissingModule modName
        Just tmap -> case Map.lookup (rawName qual) tmap of
            Nothing -> Left $ MissingType qual
            Just t  -> Right $ t
  where
    modName = qualModuleName qual

insertType :: Qual (Named Type) -> TypeEnv -> TypeEnv
insertType (Qual modName namedTree) =
    updateModule modName $
        Map.insert (rawName namedTree) (namedThing namedTree)

