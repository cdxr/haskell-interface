{-# LANGUAGE FlexibleInstances #-}

module Format where

import Data.Interface


class Format a where
    format :: a -> String

instance Format a => Format (Replace a) where
    format (Replace a b) = "   " ++ format a ++ "\n => " ++ format b

instance Format ValueChange where
    format (ValueChange r) = format r

instance Format TypeChange where
    format (TypeChange r) = format r


instance Format (Name s) where
    format = rawName

instance Format Type where
    format t = "(" ++ showType t ++ ") " ++ show t

instance Format Kind where
    format k = "(" ++ showKind k ++ ") " ++ show k

instance Format ValueDecl where
    format vd = con ++ " " ++ format (typeOf vd)
      where
        con = case vd of
                Value{}      -> "Value"
                PatternSyn{} -> "PatternSyn"
                DataCon{}    -> "DataCon"

instance Format TypeDecl where
    format td = con ++ " " ++ format (kindOf td)
      where
        con = case td of
            DataType{}  -> "DataType"
            TypeSyn{}   -> "TypeSyn"
            TypeClass{} -> "TypeClass"


instance (Format a) => Format (Named a) where
    format n = unwords
        [ show (rawName n)
        , format (origin n)
        , format (namedThing n) 
        ]

instance Format Origin where
    format o = case o of
        WiredIn       -> "[B]"
        UnknownSource -> "[?]"
        KnownSource (Source path (SrcSpan loc0 loc1)) ->
            '[' : path ++ ':' : showLoc loc0 ++ '-' : showLoc loc1 ++ "]"
      where
        showLoc (SrcLoc l c) = show l ++ ":" ++ show c
        
instance Format (Qual SomeName) where
    format qual = formatQualName qual ++ case namespace qual of
        Values -> " (value)"
        Types -> " (type)"

instance Format ClassInstance where
    format = show
