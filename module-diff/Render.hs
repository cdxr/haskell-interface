{-# LANGUAGE RecordWildCards #-}

module Render where

import Control.Monad
import Data.Tree

import Data.Interface
import Data.Interface.Change


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


renderExport :: Export -> RenderTree
renderExport e = case e of
    LocalValue q -> renderNamed valueDeclProps $ unqual q
    LocalType q  -> renderNamed typeDeclProps $ unqual q
    ReExport q   -> Node (formatSomeName q) []


renderNamed :: (a -> [RenderTree]) -> Named a -> RenderTree
renderNamed f (Named n o a) = Node n (pure (formatOrigin o) : f a)


valueDeclProps :: ValueDecl -> [RenderTree]
valueDeclProps vd = [info, Node "::" [renderType qualifyAll t] ]
  where
    t = typeOf vd

    info = case vd of
        Value{} ->
            pure "[identifier]"
        PatternSyn{} ->
            pure "[pattern synonym]"
        DataCon _ fields ->
            Node "[data constructor]" (map (pure . rawName) fields)


typeDeclProps :: TypeDecl -> [RenderTree]
typeDeclProps td = [info, Node "::" [pure $ pprintKind qualifyAll k] ]
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


renderType :: QualContext -> Type -> RenderTree
renderType qc = go
  where
    go t0 = case t0 of
        Var (TypeVar s k) -> pure s
        Con qual -> pure $ resolveQual qc qual
        Apply{} -> 
            let tcon : params = flattenApply t0
            in Node (pprintType qc tcon) (map go params)
        Fun a b ->
            Node "(->)" [go a, go b]
        Forall vs t ->
            Node "Forall "
                [ Node "[vars]" $ map (pure . pprintVar qc) vs
                , go t
                ]
        Context ps t ->
            Node "Context"
                [ Node "[preds]" $ map (pure . formatPred qc) ps
                , go t
                ]


flattenApply :: Type -> [Type]
flattenApply t0 = case t0 of
    Apply a r -> case a of
        Apply as t -> flattenApply as ++ [t,r]
        _ -> [a,r]
    _ -> [t0]
