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
renderTypeCon (TypeCon name origin kind intro) =
    Node (name ++ " :: " ++ showKind kind)
        [ Node introString [], Node (formatOrigin origin) [] ]
  where
    introString = case intro of
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
formatPred qc p = case p of
    ClassPred q t -> unwords $ [resolveQual qc q, show t]  -- TODO
    EqPred{} -> show p  -- TODO


renderExport :: QualContext -> Export -> RenderTree
renderExport qc e = case e of
    LocalValue vd -> renderValueDecl qc vd
    LocalType td  -> renderTypeDecl qc td
    ReExport q    -> Node (formatSomeName q) []


renderNamed :: (a -> [RenderTree]) -> Named a -> RenderTree
renderNamed f (Named n o a) = Node n (pure (formatOrigin o) : f a)


renderValueDecl :: QualContext -> Named ValueDecl -> RenderTree
renderValueDecl = renderNamed . valueDeclProps


renderTypeDecl :: QualContext -> Named TypeDecl -> RenderTree
renderTypeDecl = renderNamed . typeDeclProps


valueDeclProps :: QualContext -> ValueDecl -> [RenderTree]
valueDeclProps qc vd = [info, Node "::" [renderType qc t] ]
  where
    t = typeOf vd

    info = case vd of
        Value{} ->
            pure "[identifier]"
        PatternSyn{} ->
            pure "[pattern synonym]"
        DataCon _ fields ->
            Node "[data constructor]" (map (pure . rawName) fields)


typeDeclProps :: QualContext -> TypeDecl -> [RenderTree]
typeDeclProps qc td = [info, Node "::" [pure $ pprintKind qc k] ]
  where
    k = kindOf td

    info = case td of
        DataType _ Abstract ->
            pure "[abstract data type]"
        DataType _ (DataConList dataCons) ->
            Node "[data type]" (map (pure . rawName) dataCons)
        TypeSyn _ s ->
            Node "[type synonym]" [ pure s ]
        TypeClass{} ->
            pure "[class]"


renderChangedExportDiff :: QualContext -> ExportDiff -> Maybe RenderTree
renderChangedExportDiff qc ed = case ed of
    SameReExport{} -> Nothing
    DiffReExport e -> Just $ renderElem' renderReExport e
    DiffValue dv
        | isElemChanged dv ->
            Just $ renderElem (renderValueDeclChange qc)
                              (renderValueDecl qc)
                              dv
        | otherwise -> Nothing
    DiffType dt
        | isElemChanged dt ->
            Just $ renderElem (renderTypeDeclChange qc)
                              (renderTypeDecl qc)
                              dt
        | otherwise -> Nothing
  where
    renderReExport = pure . resolveQual qc


renderValueDeclChange :: QualContext -> ValueDeclChange -> RenderTree
renderValueDeclChange _ = pure . show  -- TODO

renderTypeDeclChange :: QualContext -> TypeDeclChange -> RenderTree
renderTypeDeclChange _ = pure . show  -- TODO



renderElem :: (c -> RenderTree) -> (a -> RenderTree) -> Elem c a -> RenderTree
renderElem rc ra e = case e of
    Added a   -> Node "[added]" [ ra a ]
    Removed a -> Node "[removed]" [ ra a ]
    Changed c -> rc c

renderElem' :: (a -> RenderTree) -> Elem' a -> RenderTree
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
