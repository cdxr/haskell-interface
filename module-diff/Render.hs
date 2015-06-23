{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Render where

import Control.Monad
import Data.Tree

import Data.Void ( absurd )
import Data.Functor.Foldable ( embed, project, cata, para )

import Data.Interface
import Data.Interface.Change
import Data.Interface.Type.Diff


-- TODO: add a (QualContext ->) and color information to `RenderTree`
type RenderTree = Tree String

data RenderStyle = RenderStyle
    { baseIndent :: Int
    , incrIndent :: Int
    }

defaultRenderStyle = RenderStyle
    { baseIndent = 0
    , incrIndent = 2
    }

-- | The default `RenderStyle`, except with the given base indentation level.
indent :: Int -> RenderStyle
indent i = defaultRenderStyle { baseIndent = i }


renderTreeLines :: RenderStyle -> RenderTree -> [String]
renderTreeLines RenderStyle{..} = go 0
  where
    indent d s = replicate (baseIndent + incrIndent * d) ' ' ++ s
    go depth (Node s ts) =
        indent depth s : concatMap (go $ depth + 1) ts


printRenderTree :: RenderStyle -> RenderTree -> IO ()
printRenderTree indentSize =
    putStr . unlines . renderTreeLines indentSize


renderTypeCon :: TypeCon -> RenderTree
renderTypeCon (TypeCon name origin kind info) =
    Node (name ++ " :: " ++ showKind kind)
        [ pure infoString
        , Node (formatOrigin origin) []
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


renderExport :: QualContext -> Export -> RenderTree
renderExport qc e = case e of
    LocalValue vd -> renderNamedValueDecl qc vd
    LocalType td  -> renderNamedTypeDecl qc td
    ReExport q    -> Node (formatSomeName q) []


renderNamed :: (a -> [RenderTree]) -> Named a -> RenderTree
renderNamed f (Named n a) = Node n (f a)


renderNamedValueDecl :: QualContext -> Named ValueDecl -> RenderTree
renderNamedValueDecl = renderNamed . valueDeclProps

renderNamedTypeDecl :: QualContext -> Named TypeDecl -> RenderTree
renderNamedTypeDecl = renderNamed . typeDeclProps


renderValueDeclProps :: QualContext -> ValueDecl -> [RenderTree]
renderValueDeclProps qc = valueDeclProps qc

renderTypeDeclProps :: QualContext -> TypeDecl -> [RenderTree]
renderTypeDeclProps qc = typeDeclProps qc


valueDeclProps :: QualContext -> ValueDecl -> [RenderTree]
valueDeclProps qc vd =
    [ renderValueDeclInfo $ vdInfo vd
    , Node "::" [renderType qc $ vdType vd]
    ]

renderValueDeclInfo :: ValueDeclInfo -> RenderTree
renderValueDeclInfo i = case i of
    Identifier ->
        pure "[identifier]"
    PatternSyn ->
        pure "[pattern synonym]"
    DataCon fields ->
        Node "[data constructor]" (map (pure . rawName) fields)


typeDeclProps :: QualContext -> TypeDecl -> [RenderTree]
typeDeclProps qc td =
    [ renderTypeDeclInfo $ tdInfo td
    , renderKind qc $ tdKind td
    ]

renderTypeDeclInfo :: TypeDeclInfo -> RenderTree
renderTypeDeclInfo i = case i of
    DataType Abstract ->
        pure "[abstract data type]"
    DataType (DataConList dataCons) ->
        Node "[data type]" (map (pure . rawName) dataCons)
    TypeSyn s ->
        Node "[type synonym]" [ pure s ]
    TypeClass ->
        pure "[class]"

renderKind :: QualContext -> Kind -> RenderTree
renderKind qc k = pure $ ":: " ++ pprintKind qc k


renderChangedExportDiff :: QualContext -> ExportDiff -> Maybe RenderTree
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
    renderReExport q = [pure $ resolveQual qc q]

renderElemValueDeclDiff ::
    QualContext ->
    Named (Elem ValueDeclDiff ValueDecl) ->
    RenderTree
renderElemValueDeclDiff qc =
    renderNamed $ \e ->
        [ renderElem (renderValueDeclDiff qc) (renderValueDeclProps qc) e ]

renderElemTypeDeclDiff ::
    QualContext ->
    Named (Elem TypeDeclDiff TypeDecl) ->
    RenderTree
renderElemTypeDeclDiff qc n =
    Node (rawName n)
        [ renderElem (renderTypeDeclDiff qc) (renderTypeDeclProps qc) e ]
  where
    e = unName n


renderChange :: (a -> RenderTree) -> Change a -> RenderTree
renderChange r c = case c of
    Same a -> r a   -- as if no `Change` were present
    Change a b ->
        Node "[change]"
            [ Node "-" [r a]
            , Node "+" [r b]
            ] 


renderValueDeclDiff :: QualContext -> ValueDeclDiff -> RenderTree
renderValueDeclDiff qc (ValueDeclDiff t i) =
    Node "[ValueDeclDiff]"
        [ renderChange renderValueDeclInfo i
        , renderTypeDiff qc t
        ]

renderTypeDeclDiff :: QualContext -> TypeDeclDiff -> RenderTree
renderTypeDeclDiff qc (TypeDeclDiff k i) =
    Node "[TypeDeclDiff]"
        [ renderChange renderTypeDeclInfo i
        , renderChange (renderKind qc) k
        ]


renderElem :: (c -> RenderTree) -> (a -> [RenderTree]) -> Elem c a -> RenderTree
renderElem rc ra e = case e of
    Added a   -> Node "[added]" (ra a)
    Removed a -> Node "[removed]" (ra a)
    Changed c -> rc c

renderElem' :: (a -> [RenderTree]) -> Elem' a -> RenderTree
renderElem' = renderElem absurd


renderType :: QualContext -> Type -> RenderTree
renderType qc = para (paraRenderTypeF qc)


renderTypeDiff :: QualContext -> TypeDiff -> RenderTree
renderTypeDiff qc td = case iterTypeDiff td of
    Left (Replace a b) ->
        Node "[change]"
            [ Node "-" [renderType qc a]
            , Node "+" [renderType qc b]
            ]
    Right f -> renderTypeF qc $ fmap (renderTypeDiff qc) f



-- | An algebra for reducing an open type term to a RenderTree
renderTypeF :: QualContext -> TypeF RenderTree -> RenderTree
renderTypeF qc t0 = case t0 of
    VarF (TypeVar s k) -> pure s
    ConF qual -> pure $ resolveQual qc qual
    ApplyF c a -> 
        Node "Apply" [c, a]
    --  ApplyF{} -> 
    --      let tcon : params = flattenApply t0
    --      in Node (pprintType qc tcon) params
    FunF a b ->
        Node "(->)" [a, b]
    ForallF vs t ->
        Node "Forall "
            [ Node "[vars]" $ map (pure . pprintVar qc) vs
            , t
            ]
    ContextF ps t ->
        Node "Context"
            [ Node "[preds]" $ map (pure . formatPred qc) ps
            , t
            ]

-- | A paramorphic version of `renderTypeF` that uses information about
-- subterms to pretty-print type constructors.
paraRenderTypeF :: QualContext -> TypeF (Type, RenderTree) -> RenderTree
paraRenderTypeF qc t0 = case t0 of
    ApplyF (c,_) (_, a) -> 
        Node (pprintType qc c) [a]
    _ -> renderTypeF qc $ fmap snd t0


{-
flattenApply :: Type -> [Type]
flattenApply t0 = case t0 of
    Apply a r -> case a of
        Apply as t -> flattenApply as ++ [t,r]
        _ -> [a,r]
    _ -> [t0]
-}
