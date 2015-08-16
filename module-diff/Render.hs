{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- due to TypeDiff being a synonym:
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Render where

import Control.Arrow ( second )
import Control.Applicative
import Control.Monad.Trans.Reader

import Data.Monoid
import Data.Void ( Void, absurd )
import Data.Functor.Foldable ( cata, embed )

import Data.Interface
import Data.Interface.Change
import Data.Interface.Type.Diff

import System.IO ( stdout )

import Text.PrettyPrint.ANSI.Leijen hiding ( (<>), (<$>), (<+>), (</>) )
import qualified Text.PrettyPrint.ANSI.Leijen as L


type Indent = Int

data REnv = REnv
    { reQualContext :: QualContext
    } deriving (Show)

newtype RDoc = RDoc (Reader REnv Doc)

instance Monoid RDoc where
    mempty = RDoc $ pure mempty
    mappend (RDoc a) (RDoc b) = RDoc $ liftA2 mappend a b

unRDoc :: QualContext -> RDoc -> Doc
unRDoc qc (RDoc m) = runReader m $ REnv qc

withQC :: (QualContext -> Doc) -> RDoc
withQC f = RDoc $ f <$> asks reQualContext

liftDocOp :: (Doc -> Doc -> Doc) -> RDoc -> RDoc -> RDoc
liftDocOp f a b = withQC $ \qc -> f (unRDoc qc a) (unRDoc qc b)

(<+>), (</>), (<//>), (<#>) :: RDoc -> RDoc -> RDoc
(<+>) = liftDocOp (L.<+>)
(</>) = liftDocOp (L.</>)
(<//>) = liftDocOp (L.<//>)
(<#>) = liftDocOp (L.<$>)

infixr 5 </>,<//>,<#>
infixr 6 <+>


renderToString :: (Render a) => Indent -> QualContext -> a -> String
renderToString i qc = ($ "") . renders i qc

renders :: (Render a) => Indent -> QualContext -> a -> ShowS
renders i qc a =
    displayS $ renderPretty 0.8 78 $
        indent i $ unRDoc qc $ doc a

renderStdout :: (Render a) => Indent -> QualContext -> a -> IO ()
renderStdout i qc a =
    displayIO stdout $ renderPretty 0.8 78 $
        indent i $ unRDoc qc $ doc a


-- (Deprecated)
node :: RDoc -> [RDoc] -> RDoc
node n xs = combine vcat $
    n : map (style $ indent 2) xs

node' :: String -> [RDoc] -> RDoc
node' = node . text'

style :: (Doc -> Doc) -> RDoc -> RDoc
style f (RDoc m) = RDoc $ fmap f m

combine :: (Functor f) => (f Doc -> Doc) -> f RDoc -> RDoc
combine f ds = withQC $ \qc -> f $ fmap (unRDoc qc) ds

text' :: String -> RDoc
text' = doc . text

char' :: Char -> RDoc
char' = doc . char

qual :: Qual String -> RDoc
qual q = RDoc $ do
    qc <- asks reQualContext
    pure $ text $ resolveQual qc q



class Render a where
    doc :: a -> RDoc
    doc = RDoc . pure . doc'

    doc' :: a -> Doc
    doc' = unRDoc qualifyAll . doc

    namedDoc :: Named a -> RDoc
    namedDoc (Named n a) =
        text' n <#> style (indent 2) (doc a)

    {-# MINIMAL doc | doc' #-}


instance Render RDoc where
    doc = id
    {-# INLINABLE doc #-}

instance Render Doc where
    doc' = id
    {-# INLINABLE doc' #-}

instance Render Void where
    doc = absurd

instance Render TypeConInfo where
    doc i = text' $ case i of
        ConAlgebraic -> "[algebraic]"
        ConSynonym   -> "[synonym]"
        ConClass     -> "[class]"


instance Render TypeCon where
    doc (TypeCon name origin kind info) =
        -- TODO
        node (text' name <+> prefixSig (doc kind))
            [ doc info
            , node (text' $ formatOrigin origin) []
            ]


formatOrigin :: Origin -> String
formatOrigin o = case o of
    WiredIn       -> "[built-in]"
    UnknownSource -> "[??? source unknown]"
    KnownSource (Source path (SrcSpan loc0 loc1)) ->
        '[' : path ++ ':' : showLoc loc0 ++ '-' : showLoc loc1 ++ "]"
  where
    showLoc (SrcLoc l c) = show l ++ ":" ++ show c


formatPred :: (Render a) => Pred a -> RDoc
formatPred p = case p of
    ClassPred ts -> combine hsep $ map doc ts
    EqPred{} -> undefined --text' (show p)  -- TODO



instance Render Export where
    doc (Named n e) = case e of
        LocalValue vd -> namedDoc $ Named n vd
        LocalType td  -> namedDoc $ Named n td
        ReExport m _ns ->
            qual (Qual m n) <> style (indent 2) (text' "(re-export)")


renderIfChanged :: (Diff a c, Render c) => c -> RDoc
renderIfChanged d
    | isChanged d = doc d
    | otherwise = mempty


-- Declarations

instance Render ModuleInterface where
    doc iface = combine vcat
        [ text' $ "Module: " ++ moduleName iface
        ]


instance Render ValueDecl where
    doc (ValueDecl t i) =
        doc i <#> prefixSig (doc t) <> doc line

instance Render ValueDeclDiff where
    doc (ValueDeclDiff t i) =
        doc i <#> prefixSig (doc t) <> doc line


instance Render ValueDeclInfo where
    doc i = case i of
        Identifier ->
            text' "[id]"
        PatternSyn ->
            text' "[pattern]"
        DataCon fields ->
            node' "[data con]" (map (text' . rawName) fields)


instance Render TypeDecl where
    doc (TypeDecl k i) =
        doc i <#> prefixSig (doc k) <> doc line

instance Render TypeDeclDiff where
    doc (TypeDeclDiff k i) =
        doc i <#> prefixSig (doc k) <> doc line

instance Render TypeDeclInfo where
    doc i = case i of
        DataType Abstract ->
            text' "[abstract data type]"
        DataType (DataConList dataCons) ->
            node' "[data type]" (map (text' . rawName) dataCons)
        TypeSyn s ->
            node' "[synonym]" [ text' s ]
        TypeClass ->
            text' "[class]"


instance Render Kind where
    doc k = case k of
        KindVar s -> text' s
        StarKind -> char' '*'
        HashKind -> char' '#'
        SuperKind -> text' "BOX"
        ConstraintKind -> text' "Constraint"
        PromotedType q -> qual q
        FunKind a b -> doc a <+> text' "->" </> doc b

{-
instance Render ExportDiff where
    doc ed = case ed of
        LocalValueDiff dv -> namedDoc dv
        LocalTypeDiff dt -> namedDoc dt
        ExportDiff c -> doc c
-}
        

-- TODO: make ExportDiff an instance of Diff:
--renderChangedExportDiff = renderIfChanged


prefixSig :: RDoc -> RDoc
prefixSig d = text' "::" <+> style align d


instance (Render c, Render a) => Render (Elem c a) where
    doc e = case e of
        Added a   -> style green $ doc a
        Removed b -> style red $ doc b
        Elem c    -> doc c

    namedDoc (Named n e) = case e of
        Added a   -> style green $ namedDoc (Named n a)
        Removed b -> style red $ namedDoc (Named n b)
        Elem c    -> namedDoc (Named n c)


instance (Render a) => Render (Change a) where
    doc c = case c of
        NoChange a -> doc a
        Change a b ->
            combine vcat
                [ style red $ text' "-" <+> style align (doc a)
                , style green $ text' "+" <+> style align (doc b)
                ]

instance (Render a) => Render (Replace a) where
    doc = doc . toChange
     


instance Render Type where
    doc = snd . renderTypePrec

instance Render TypeDiff where
    doc = snd . renderTypeDiffPrec


data Prec = TopPrec | FunPrec | AppPrec | ConPrec
    deriving (Show, Eq, Ord)

renderTypePrec :: Type -> (Prec, RDoc)
renderTypePrec = cata renderTypeAlg

renderTypeAlg :: (Render a) => TypeF (Prec, a) -> (Prec, RDoc)
renderTypeAlg = undefined  -- XXX TODO
{-
renderTypeAlg t0 = case t0 of
    VarF (TypeVar s _) -> (ConPrec, text' s)
    ConF q -> (ConPrec, qual q)
    ApplyF c a ->
        (,) AppPrec $ docPrec TopPrec c <+> docPrec AppPrec a
    FunF a b ->
        (,) FunPrec $
        docPrec FunPrec a <+> text' "->" </> docPrec TopPrec b
    ForallF vs (_, t) ->
        (,) TopPrec $
        if showForall
            then doc (renderForall vs) <#> doc t
            else doc t
    ContextF ps (_, t) ->
        (,) TopPrec $
            combine tupled (map formatPred ps) <+> text' "=>" </> doc t
  where
    showForall = False

    docPrec :: (Render a) => Prec -> (Prec, a) -> RDoc
    docPrec prec0 (prec, d)
         | prec <= prec0 = style parens (doc d)
         | otherwise = doc d
-}


renderForall :: [TypeVar] -> Doc
renderForall vs = hsep (map text $ "forall" : varNames) <> dot
  where
    varNames = map varName vs


renderTypeDiffPrec :: TypeDiff -> (Prec, RDoc)
renderTypeDiffPrec = cata renderTypeDiffAlg
    
renderTypeDiffAlg :: (Render a) => DiffTypeF Type (Prec, a) -> (Prec, RDoc)
renderTypeDiffAlg td0 = case td0 of
    NoDiffTypeF t -> (ConPrec, doc $ embed t)
    ReplaceTypeF t0 t1 ->
        (,) ConPrec $ style braces $
            style red (doc $ embed t0) <+>
            text' "/" <+>
            style green (doc $ embed t1)
    SameTypeF fc -> renderTypeAlg $ fmap (second doc) fc
  where
    replaceDoc :: Replace Type -> RDoc
    replaceDoc (Replace t0 t1) =
        style braces $
            style red (doc t0) <+> text' "/" <+> style green (doc t1)
