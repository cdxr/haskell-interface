{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Interface.Type.Pretty
(
    prettyPrintType
  , QualContext
  , qualifyAll
)
where

import Control.Monad.Trans.Reader

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Monoid

import Data.Interface.Name
import Data.Interface.Type.Type


prettyPrintType :: QualContext -> Type -> String
prettyPrintType qc t = runTypeFormatter (formatType t) qc TopPrec ""


type QualContext = Set ModuleName

qualifyAll :: QualContext
qualifyAll = Set.empty

qualContextFormat :: (HasRawName n) => QualContext -> Qual n -> String
qualContextFormat qc qualName@(Qual modName n)
    | modName `Set.member` qc = rawName n
    | otherwise = formatQualName qualName


-- | Precedence levels for pretty-printing a type
data Prec
    = TopPrec
    | FunPrec
    | InfixPrec
    | ApplyPrec
    deriving (Show, Eq, Ord)
-- note: ^ this type is copied almost verbatim from TyPrec in GHC
    


newtype TF a = TF { tfReader :: Reader (QualContext, Prec) a }
    deriving (Functor, Applicative, Monad)

type TypeFormatter = TF ShowS

runTypeFormatter :: TF a -> QualContext -> Prec -> a
runTypeFormatter (TF m) qc p = runReader m (qc, p)


--instance Monoid TypeFormatter where
instance Monoid (TF (a -> a)) where
    mempty = TF $ pure id
    mappend (TF a) (TF b) = TF $ (.) <$> a <*> b

qualName :: (HasRawName n) => Qual n -> TypeFormatter
qualName q = do
    qc <- TF $ asks fst
    string $ qualContextFormat qc q 

string :: String -> TypeFormatter
string = TF . pure . showString


-- TODO: remove qualifiers
-- TODO: special cases for wired types
formatType :: Type -> TypeFormatter
formatType t0 = case t0 of
    Var v   -> string $ varName v
--  Wired w -> string $ showWiredType w
    Con q   -> string $ formatQualName q
    Link q  -> string $ formatQualName q
    Apply c t -> formatType c <> string " " <> formatType t
    Fun a b -> formatType a <> string " -> " <> formatType b
    Forall vs t -> 
        string (unwords $ "forall" : map varName vs) <>
            string ". " <> formatType t


showWiredType :: WiredType -> String
showWiredType w = case w of
    WBool     -> "Bool"
    WEq       -> "Eq"
    WOrdering -> "Ordering"
    WChar     -> "Char"
    WDouble   -> "Double"
    WFloat    -> "Float"
    WInt      -> "Int"
    WWord     -> "Word"
    WList     -> "[]"
    WUnit     -> "()"
    WTuple a
        | a < 2 || a > 20 ->
            error $ "showWiredType: tuple with arity " ++ show a
        | otherwise -> '(' : replicate (a-1) ',' ++ ")"
