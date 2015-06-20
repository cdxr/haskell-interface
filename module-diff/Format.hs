{-# LANGUAGE FlexibleInstances #-}

module Format where

import Control.Monad
import Data.Tree

import Data.Interface
import Data.Interface.Change


type FormatTree = Tree String

formatNode :: String -> [FormatTree] -> FormatTree
formatNode = Node

printFormatTree :: Int -> FormatTree -> IO ()
printFormatTree indentSize = go 0
  where
    indent n s = replicate n ' ' ++ s
    go depth (Node s ts) = do
        putStrLn $ indent (depth * indentSize) s
        forM_ ts $ go (depth + 1)

prepend :: String -> FormatTree -> FormatTree
prepend s (Node a ts) = Node (s ++ a) ts


class Format a where
    format :: a -> Tree String

subFormat :: Format a => a -> Forest String
subFormat = subForest . format


instance Format a => Format (Replace a) where
    format (Replace a b) =
        Node "Replace"
            [ prepend "from: " (format a)
            , prepend "to:   " (format b)
            ]

instance Format ValueDeclChange where
    format (ValueDeclChange r) = format r

instance Format TypeDeclChange where
    format (TypeDeclChange r) = format r


instance Format (Name s) where
    format = pure . rawName

instance Format Type where
    format t =
        Node "[Type]"
            --[ pure $ "show: " ++ show t
            --, pure $ "render: " ++ pprintType qualifyAll t
            [ typeTreeFormat qualifyAll t
            ]

instance Format Kind where
    format k = 
        Node ("[Kind]  " ++ showKind k) []

instance Format ValueDecl where
    format vd =
        Node lbl [format $ typeOf vd]
      where
        lbl = case vd of
                Value{}      -> "Value"
                PatternSyn{} -> "PatternSyn"
                DataCon _ fs -> "DataCon " ++ show (map rawName fs)


instance Format TypeDecl where
    format td = 
        Node lbl [format $ kindOf td]
      where
        lbl = case td of
            DataType{}  -> "DataType"
            TypeSyn{}   -> "TypeSyn"
            TypeClass{} -> "TypeClass"


instance (Format a) => Format (Named a) where
    format n =
        Node (rawName n)
            [ format (origin n)
            , format (namedThing n) 
            ]

instance Format Origin where
    format o = pure $ case o of
        WiredIn       -> "[B]"
        UnknownSource -> "[?]"
        KnownSource (Source path (SrcSpan loc0 loc1)) ->
            '[' : path ++ ':' : showLoc loc0 ++ '-' : showLoc loc1 ++ "]"
      where
        showLoc (SrcLoc l c) = show l ++ ":" ++ show c
        
instance Format (Qual SomeName) where
    format qual = pure $ showQualName qual ++ case namespace qual of
        Values -> " (value)"
        Types -> " (type)"

instance Format ClassInstance where
    format = pure . show


typeTreeFormat :: QualContext -> Type -> FormatTree
typeTreeFormat qc = go
  where
    go t0 = case t0 of
        Var (TypeVar s k) -> pure s
        Con qual -> pure $ resolveQual qc qual
        Link qual -> pure $ resolveQual qc qual
        Apply{} -> 
            let tcon : params = flattenApply t0
            in formatNode (pprintType qc tcon) (map go params)
        Fun a b ->
            formatNode "(->)" [go a, go b]
        Forall vs t ->
            formatNode "Forall "
                [ formatNode "[vars]" $ map (pure . pprintVar qc) vs
                , go t
                ]

flattenApply :: Type -> [Type]
flattenApply t0 = case t0 of
    Apply a r -> case a of
        Apply as t -> flattenApply as ++ [t,r]
        _ -> [a,r]
    _ -> [t0]
