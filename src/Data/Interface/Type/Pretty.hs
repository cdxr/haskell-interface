{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Interface.Type.Pretty
(
    pprintType
  , pprintKind
  , pprintVar
  , pprintPred
  , QualContext
  , defQualContext
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
pprintType = makePrinter formatType

pprintKind :: QualContext -> Kind -> String
pprintKind = makePrinter formatKind

pprintVar :: QualContext -> TypeVar -> String
pprintVar = makePrinter formatVar

pprintPred :: QualContext -> Pred -> String
pprintPred = makePrinter formatPred


makePrinter :: (a -> TypeFormatter) -> QualContext -> a -> String
makePrinter f qc a = runTypeFormatter (f a) qc TopPrec ""


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


fromShowS :: ShowS -> TypeFormatter
fromShowS = TF . pure


paren :: Prec -> TypeFormatter -> TypeFormatter
paren p tf = do
    thisPrec <- TF $ asks snd
    showParen (thisPrec > p) <$> tf


qualName :: (HasRawName n) => Qual n -> TypeFormatter
qualName q = do
    qc <- TF $ asks fst
    string $ resolveQual qc q 

string :: String -> TypeFormatter
string = fromShowS . showString

char :: Char -> TypeFormatter
char = fromShowS . showChar

formatWords :: [TypeFormatter] -> TypeFormatter
formatWords = mconcat . intersperse (char ' ')

formatTuple :: [TypeFormatter] -> TypeFormatter
formatTuple fs =
    char '(' <> mconcat (intersperse (string ", ") fs) <> char ')'


-- TODO: infix type constructors
formatType :: Type -> TypeFormatter
formatType t0 = case t0 of
    Con q   -> qualName q
    Apply c t -> formatApply c t
    Fun a b -> formatFun a b
    Var v   -> string $ varName v
    Forall vs t ->
        formatWords (string "forall" : map formatForallVar vs) <>
            string ". " <> formatType t
    Context ps t -> cxt <> string " => " <> formatType t
      where cxt = case ps of
                    []  -> string "()"
                    [p] -> formatPred p
                    _   -> formatTuple (map formatPred ps)


formatPred :: Pred -> TypeFormatter
formatPred p = case p of
    ClassPred q ts -> formatWords $ qualName q : map formatType ts
    EqPred{} -> string (show p)  -- TODO



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
