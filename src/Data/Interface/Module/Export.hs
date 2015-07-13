module Data.Interface.Module.Export where


import Data.Maybe ( catMaybes )

import Data.Interface.Name

import Data.Interface.Module.Decl


data ExportName
    = LocalName SomeName
    | ForeignName (Qual SomeName)
    deriving (Show, Eq, Ord)

{- ExportName: We avoid qualifying local exports because when it comes time
   to compare the exports of two modules, it shouldn't matter if the modules
   have the same name. The `Eq` instance will match up local names with local
   names.
-}

instance HasRawName ExportName where
    rawName en = case en of
        LocalName n -> rawName n
        ForeignName q -> rawName q
    rename f en = case en of
        LocalName n -> LocalName $ rename f n
        ForeignName q -> ForeignName $ rename f q

instance TraverseNames ExportName where
    traverseNames f en = case en of
        LocalName sn  -> LocalName <$> traverseNames f sn
        ForeignName q -> ForeignName <$> traverseNames f q



data Export
    = LocalValue (Named ValueDecl)
    | LocalType (Named TypeDecl)
    | ReExport (Qual SomeName)
    deriving (Show, Eq, Ord)

exportName :: Export -> ExportName
exportName e = case e of
    LocalValue n -> LocalName (someName n)
    LocalType n  -> LocalName (someName n)
    ReExport q   -> ForeignName q


-- | Produce a triple containing a list of all export names, a list of all
-- value declarations, and a list of all type declarations.
splitExports ::
    [Export] ->
    ([ExportName], [Named ValueDecl], [Named TypeDecl])
splitExports es =
    let (names, mvals, mtypes) = unzip3 $ map makeTup es
    in (names, catMaybes mvals, catMaybes mtypes)
  where
    makeTup e = case e of
        LocalValue a -> (exportName e, Just a, Nothing)
        LocalType a  -> (exportName e, Nothing, Just a)
        ReExport{}   -> (exportName e, Nothing, Nothing)

