{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Render where

import Control.Monad
import Control.Monad.Free

import Data.Void ( absurd )
import Data.Functor.Foldable ( para )

import Data.Interface
import Data.Interface.Change
import Data.Interface.Type.Diff

import System.Console.ANSI as ANSI


data Render a
    = Render String [a]
    | SetSGR [ANSI.SGR] a
    deriving (Functor, Foldable, Traversable)


data RenderStyle = RenderStyle
    { baseIndent :: Int
    , incrIndent :: Int
    }

defaultRenderStyle :: RenderStyle
defaultRenderStyle = RenderStyle
    { baseIndent = 0
    , incrIndent = 2
    }

-- | The default `RenderStyle`, except with the given base indentation level.
indent :: Int -> RenderStyle
indent i = defaultRenderStyle { baseIndent = i }


type RenderTree = Free Render ()

renderStdout :: RenderStyle -> RenderTree -> IO ()
renderStdout style renderTree = do
    let initSGR = [ANSI.Reset]
    putStr $ ANSI.setSGRCode initSGR
    go 0 initSGR renderTree
  where
    go :: Int -> [ANSI.SGR] -> Free Render () -> IO ()
    go depth sgr0 m0 = case m0 of
        Pure () -> pure ()
        Free r -> case r of
            Render s ms -> do
                putStrLn $ indentString depth s
                forM_ ms $ go (depth+1) sgr0
            SetSGR sgr m -> do
                putStr $ setSGRCode sgr
                go depth sgr m
                putStr $ setSGRCode sgr0

    indentString :: Int -> String -> String
    indentString d s = replicate (baseIndent + incrIndent * d) ' ' ++ s
      where RenderStyle{..} = style


node :: (MonadFree Render m) => String -> [m ()] -> m ()
node s = wrap . Render s

line :: (MonadFree Render m) => String -> m ()
line s = wrap $ Render s []

color :: ANSI.Color -> Free Render a -> Free Render a
color c = wrap . SetSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]


renderTypeCon :: TypeCon -> Free Render ()
renderTypeCon (TypeCon name origin kind info) =
    node (name ++ " :: " ++ showKind kind)
        [ line infoString
        , node (formatOrigin origin) []
        ]
  where
    infoString = case info of
        ConAlgebraic -> "[algebraic]"
        ConSynonym   -> "[synonym]"
        ConClass     -> "[class]"


formatOrigin :: Origin -> String
formatOrigin o = case o of
    WiredIn       -> "[built-in]"
    UnknownSource -> "[??? source unknown]"
    KnownSource (Source path (SrcSpan loc0 loc1)) ->
        '[' : path ++ ':' : showLoc loc0 ++ '-' : showLoc loc1 ++ "]"
  where
    showLoc (SrcLoc l c) = show l ++ ":" ++ show c

formatSomeName :: Qual SomeName -> String
formatSomeName q = showQualName q ++ case namespace q of
    Values -> " (value)"
    Types -> " (type)"

formatPred :: QualContext -> Pred -> String
formatPred = pprintPred


renderExport :: QualContext -> Export -> Free Render ()
renderExport qc e = case e of
    LocalValue vd -> renderNamedValueDecl qc vd
    LocalType td  -> renderNamedTypeDecl qc td
    ReExport q    -> node (formatSomeName q) []


renderNamed :: (a -> [Free Render ()]) -> Named a -> Free Render ()
renderNamed f (Named n a) = node n (f a)


renderNamedValueDecl :: QualContext -> Named ValueDecl -> Free Render ()
renderNamedValueDecl = renderNamed . valueDeclProps

renderNamedTypeDecl :: QualContext -> Named TypeDecl -> Free Render ()
renderNamedTypeDecl = renderNamed . typeDeclProps


renderValueDeclProps :: QualContext -> ValueDecl -> [Free Render ()]
renderValueDeclProps qc = valueDeclProps qc

renderTypeDeclProps :: QualContext -> TypeDecl -> [Free Render ()]
renderTypeDeclProps qc = typeDeclProps qc


valueDeclProps :: QualContext -> ValueDecl -> [Free Render ()]
valueDeclProps qc vd =
    [ renderValueDeclInfo $ vdInfo vd
    , node "::" [renderType qc $ vdType vd]
    ]

renderValueDeclInfo :: ValueDeclInfo -> Free Render ()
renderValueDeclInfo i = case i of
    Identifier ->
        line "[identifier]"
    PatternSyn ->
        line "[pattern synonym]"
    DataCon fields ->
        node "[data constructor]" (map (line . rawName) fields)


typeDeclProps :: QualContext -> TypeDecl -> [Free Render ()]
typeDeclProps qc td =
    [ renderTypeDeclInfo $ tdInfo td
    , renderKind qc $ tdKind td
    ]

renderTypeDeclInfo :: TypeDeclInfo -> Free Render ()
renderTypeDeclInfo i = case i of
    DataType Abstract ->
        line "[abstract data type]"
    DataType (DataConList dataCons) ->
        node "[data type]" (map (line . rawName) dataCons)
    TypeSyn s ->
        node "[type synonym]" [ line s ]
    TypeClass ->
        line "[class]"

renderKind :: QualContext -> Kind -> Free Render ()
renderKind qc k = line $ ":: " ++ pprintKind qc k


renderChangedExportDiff :: QualContext -> ExportDiff -> Maybe (Free Render ())
renderChangedExportDiff qc ed = case ed of
    SameReExport{} -> Nothing
    DiffReExport e -> Just $ renderElem' renderReExport e
    DiffValue dv
        | isElemChanged (unName dv) -> Just $ renderElemValueDeclDiff qc dv
        | otherwise -> Nothing
    DiffType dt
        | isElemChanged (unName dt) -> Just $ renderElemTypeDeclDiff qc dt
        | otherwise -> Nothing
  where
    renderReExport q = [line $ resolveQual qc q]

renderElemValueDeclDiff ::
    QualContext ->
    Named (Elem ValueDeclDiff ValueDecl) ->
    Free Render ()
renderElemValueDeclDiff qc =
    renderNamed $ \e ->
        [ renderElem (renderValueDeclDiff qc) (renderValueDeclProps qc) e ]

renderElemTypeDeclDiff ::
    QualContext ->
    Named (Elem TypeDeclDiff TypeDecl) ->
    Free Render ()
renderElemTypeDeclDiff qc n =
    node (rawName n)
        [ renderElem (renderTypeDeclDiff qc) (renderTypeDeclProps qc) e ]
  where
    e = unName n


renderChange :: (a -> Free Render ()) -> Change a -> Free Render ()
renderChange r c = case c of
    Same a -> r a   -- as if no `Change` were present
    Change a b ->
        node "[change]"
            [ color ANSI.Red $ node "-" [r a]
            , color ANSI.Green $ node "+" [r b]
            ] 


renderValueDeclDiff :: QualContext -> ValueDeclDiff -> Free Render ()
renderValueDeclDiff qc (ValueDeclDiff t i) =
    node "[ValueDeclDiff]"
        [ renderChange renderValueDeclInfo i
        , renderTypeDiff qc t
        ]

renderTypeDeclDiff :: QualContext -> TypeDeclDiff -> Free Render ()
renderTypeDeclDiff qc (TypeDeclDiff k i) =
    node "[TypeDeclDiff]"
        [ renderChange renderTypeDeclInfo i
        , renderChange (renderKind qc) k
        ]


renderElem ::
    (c -> Free Render ()) ->
    (a -> [Free Render ()]) ->
    Elem c a -> Free Render ()
renderElem rc ra e = case e of
    Added a   -> color ANSI.Green $ node "[added]" (ra a)
    Removed a -> color ANSI.Red $ node "[removed]" (ra a)
    Changed c -> rc c

renderElem' :: (a -> [Free Render ()]) -> Elem' a -> Free Render ()
renderElem' = renderElem absurd


renderType :: QualContext -> Type -> Free Render ()
renderType qc = para (paraRenderTypeF qc)


renderTypeDiff :: QualContext -> TypeDiff -> Free Render ()
renderTypeDiff qc td = case iterTypeDiff td of
    Left (Replace a b) ->
        node "[change]"
            [ color ANSI.Red $ node "-" [renderType qc a]
            , color ANSI.Green $ node "+" [renderType qc b]
            ]
    Right f -> renderTypeF qc $ fmap (renderTypeDiff qc) f



-- | An algebra for reducing an open type term to a Free Render ()
renderTypeF :: QualContext -> TypeF (Free Render ()) -> Free Render ()
renderTypeF qc t0 = case t0 of
    VarF (TypeVar s _) -> line s
    ConF qual -> line $ resolveQual qc qual
    ApplyF c a -> 
        node "Apply" [c, a]
    --  ApplyF{} -> 
    --      let tcon : params = flattenApply t0
    --      in node (pprintType qc tcon) params
    FunF a b ->
        node "(->)" [a, b]
    ForallF vs t ->
        node "Forall "
            [ node "[vars]" $ map (line . pprintVar qc) vs
            , t
            ]
    ContextF ps t ->
        node "Context"
            [ node "[preds]" $ map (line . formatPred qc) ps
            , t
            ]

-- | A paramorphic version of `renderTypeF` that uses information about
-- subterms to pretty-print type constructors.
paraRenderTypeF ::
    QualContext ->
    TypeF (Type, Free Render ()) ->
    Free Render ()
paraRenderTypeF qc t0 = case t0 of
    ApplyF (c,_) (_, a) -> 
        node (pprintType qc c) [a]
    _ -> renderTypeF qc $ fmap snd t0


{-
flattenApply :: Type -> [Type]
flattenApply t0 = case t0 of
    Apply a r -> case a of
        Apply as t -> flattenApply as ++ [t,r]
        _ -> [a,r]
    _ -> [t0]
-}
