{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Interface.Type.Pretty
(
    pprintType
  , pprintKind
  , pprintVar
  , QualContext
  , qualifyAll
)
where

import Control.Monad.Trans.Reader

import Data.List ( intersperse )

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Monoid

import Data.Interface.Name
import Data.Interface.Type.Type


pprintType :: QualContext -> Type -> String
pprintType qc t = runTypeFormatter (formatType t) qc TopPrec ""

pprintKind :: QualContext -> Kind -> String
pprintKind qc k = runTypeFormatter (formatKind k) qc TopPrec ""

pprintVar :: QualContext -> TypeVar -> String
pprintVar qc v = runTypeFormatter (formatVar v) qc TopPrec ""


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

-- | Embed a TypeFormatter with the given precedence.
prec :: Prec -> TypeFormatter -> TypeFormatter
prec p (TF m) = TF $ local setPrec m
  where
    setPrec (qc, _) = (qc, p)


paren :: Prec -> TypeFormatter -> TypeFormatter
paren p tf = do
    thisPrec <- TF $ asks snd
    showParen (thisPrec > p) <$> tf


qualName :: (HasRawName n) => Qual n -> TypeFormatter
qualName q = do
    qc <- TF $ asks fst
    string $ resolveQual qc q 

string :: String -> TypeFormatter
string = TF . pure . showString

char :: Char -> TypeFormatter
char = TF . pure . showChar

formatWords :: [TypeFormatter] -> TypeFormatter
formatWords = mconcat . intersperse (char ' ')


-- TODO: special cases for wired types
-- TODO: infix type constructors
formatType :: Type -> TypeFormatter
formatType t0 = case t0 of
    Var v   -> string $ varName v
--  Wired w -> string $ showWiredType w
    Con q   -> qualName q
    Apply c t -> formatApply c t
    Fun a b -> formatFun a b
    Forall vs t ->
        formatWords (string "forall" : map formatForallVar vs) <>
            string ". " <> formatType t


-- | Format a type constructor applied to a parameter
formatApply :: Type -> Type -> TypeFormatter
formatApply c t = case t of
    Var (TypeVar _ SuperKind) -> formatType c   -- hide kind parameters
    _ -> paren ApplyPrec $ prec FunPrec (formatType c)
                        <> char ' '
                        <> prec ApplyPrec (formatType t)


formatFun :: Type -> Type -> TypeFormatter
formatFun a b =
    paren FunPrec $ prec FunPrec (formatType a)
                 <> string " -> "
                 <> formatType b


-- | Format a type variable with a kind signature
formatVar :: TypeVar -> TypeFormatter
formatVar (TypeVar s k) =
    char '(' <> string (s ++ " :: ") <> formatKind k <> char ')'


-- | Format a TypeVar as it should be seen in a "forall" list. Each type variable
-- is rendered with its kind signature, except when the kind is "*".
formatForallVar :: TypeVar -> TypeFormatter
formatForallVar v@(TypeVar s k) = case k of
    StarKind -> string s
    _        -> formatVar v


formatKind :: Kind -> TypeFormatter
formatKind k = case k of
    KindVar s -> string s
    StarKind -> char '*'
    HashKind -> char '#'
    SuperKind -> string "BOX"
    ConstraintKind -> string "Constraint"
    PromotedType q -> qualName q
    FunKind a b -> formatKind a <> string " -> " <> formatKind b


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
