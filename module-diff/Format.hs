{-# LANGUAGE FlexibleInstances #-}

module Format where

import Control.Monad
import Data.Tree

import Data.Interface


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

instance Format ValueChange where
    format (ValueChange r) = format r

instance Format TypeChange where
    format (TypeChange r) = format r


instance Format (Name s) where
    format = pure . rawName

instance Format Type where
    format t =
        Node "[Type]"
            [ pure $ "show: " ++ show t
            , pure $ "render: " ++ prettyPrintType qualifyAll t
            ]

instance Format Kind where
    format k = 
        Node "[Kind]"
            [ pure $ "show: " ++ show k
            , pure $ "render: " ++ showKind k
            ]

instance Format ValueDecl where
    format vd =
        Node lbl $ subFormat (typeOf vd)
      where
        lbl = case vd of
                Value{}      -> "Value"
                PatternSyn{} -> "PatternSyn"
                DataCon{}    -> "DataCon"

instance Format TypeDecl where
    format td = 
        Node lbl $ subFormat (kindOf td)
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
    format qual = pure $ formatQualName qual ++ case namespace qual of
        Values -> " (value)"
        Types -> " (type)"

instance Format ClassInstance where
    format = pure . show
