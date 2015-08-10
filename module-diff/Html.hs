{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid
import Data.Foldable

import qualified Data.ByteString.Lazy as BS
import qualified Blaze.ByteString.Builder as Blaze

import Data.Text ( Text )
import qualified Data.Text as Text

import Data.Interface
import Data.Interface.Change
import Data.Interface.Type.Render

import Lucid

import Task
import Program
import ProgramArgs
--import Render
import Style


runTask :: ProgramResult -> Program ()
runTask r = do
    mfp <- getArg outputFile
    let output = case mfp of
            Nothing -> stdoutHtml
            Just fp -> writeHtml fp

    output $ case r of
        AModule tgt m -> renderModulePage tgt m
        APackage p -> renderPackagePage p
        AModuleDiff tgt0 tgt1 mdiff -> renderModuleDiffPage tgt0 tgt1 mdiff
        APackageDiff pdiff -> renderPackageDiffPage pdiff


stdoutHtml :: HtmlT Program () -> Program ()
stdoutHtml = liftIO . BS.putStr <=< renderBST

writeHtml :: FilePath -> HtmlT Program () -> Program ()
writeHtml fp = liftIO . BS.writeFile fp . Blaze.toLazyByteString <=< execHtmlT

writePackageHtml :: FilePath -> PackageInterface -> Program ()
writePackageHtml fp = writeHtml fp . renderPackagePage


simplePage :: (Monad m) => String -> HtmlT m () -> HtmlT m ()
simplePage titleString html = doctypehtml_ $ do
    let title = toHtml titleString
    head_ $ do
        title_ title
        style_ [ type_ "text/css" ] Style.mainStyleText
    body_ $ do
        h1_ [ class_ "title" ] title
        html


renderPackagePage :: PackageInterface -> HtmlT Program ()
renderPackagePage iface = do
    let title = showPackageId (pkgId iface)
    simplePage title $
        renderModuleGroup . toList $ pkgExposedModules iface


createLink ::
    (Monad m, Monad n) =>
    Text ->                  -- ^ unique element id
    HtmlT m a ->             -- ^ hyperlink markup
    HtmlT n b ->             -- ^ linked content
    (HtmlT m a, HtmlT n b)
createLink uniqueId link content =
    ( a_ [ href_ (Text.pack "#" <> uniqueId) ] link
    , with content [ id_ uniqueId ]
    )

createTextLink ::
    (Monad m) =>
    (a -> Text) ->               -- ^ create unique id
    (a -> String) ->             -- ^ create link text
    (a -> HtmlT m b) ->          -- ^ create content
    a -> (HtmlT m (), HtmlT m b)
createTextLink mkId mkText mkContent a =
    createLink (mkId a) (toHtml $ mkText a) (mkContent a)


simpleLinkList :: (Monad m) =>
    (a -> Text) ->
    (a -> String) ->
    (a -> HtmlT m b) ->
    [a] -> HtmlT m ()
simpleLinkList mkId mkText mkContent xs = do
    let (links, sections) =
            unzip $ map (createTextLink mkId mkText mkContent) xs

    ul_ [ class_ "links" ] $ mapM_ (li_ [ class_ "link" ]) links

    div_ $ sequence_ sections


renderModulePage :: ModuleTarget -> ModuleInterface -> HtmlT Program ()
renderModulePage t = simplePage (moduleTargetString t) . renderModuleInterface

renderModuleDiffPage ::
    ModuleTarget -> ModuleTarget -> ModuleDiff -> HtmlT Program ()
renderModuleDiffPage t0 t1 mdiff =
    simplePage title $ do
        ol_ $ do
            li_ [ class_ "module-source" ] $ toHtml $ moduleTargetString t0
            li_ [ class_ "module-source" ] $ toHtml $ moduleTargetString t1

        renderModuleDiff mdiff
  where
    Change name0 name1 = moduleName <$> toChange mdiff
    title | name0 == name1 = name1 ++ " (version comparison)"
          | otherwise = name0 ++ " / " ++ name1 ++ " (module comparison)"


renderPackageDiffPage :: PackageDiff -> HtmlT Program ()
renderPackageDiffPage pdiff = simplePage title (renderPackageDiff pdiff)
  where
    title = case diffPkgId pdiff of
        NoChange pid -> showPackageId pid
        Change a b -> showPackageId a ++ " / " ++ showPackageId b



renderModuleGroup :: [ModuleInterface] -> HtmlT Program ()
renderModuleGroup = simpleLinkList makeId makeLinkText renderModuleInterface
  where
    makeId :: ModuleInterface -> Text
    makeId = moduleNameElemId . moduleName

    makeLinkText :: ModuleInterface -> String
    makeLinkText = moduleName


-- | Format a `ModuleName` as a legal html element id.
moduleNameElemId :: ModuleName -> Text
moduleNameElemId = Text.pack . map replaceDot
  where
    replaceDot c | c == '.' = '-'
                 | otherwise = c


renderModuleDiffGroup :: [Elem ModuleDiff ModuleInterface] -> HtmlT Program ()
renderModuleDiffGroup = simpleLinkList makeId makeLinkText render
  where
    makeId :: Elem ModuleDiff ModuleInterface -> Text
    makeId e = case e of
        Removed m -> "removed-" <> moduleNameElemId (moduleName m)
        Added m   -> "added-" <> moduleNameElemId (moduleName m)
        Elem c    -> Text.pack $ new $ diffModuleName c

    makeLinkText :: Elem ModuleDiff ModuleInterface -> String
    makeLinkText e = case e of
        Removed m -> moduleName m ++ "  (removed)"
        Added m   -> moduleName m ++ "  (new)"
        Elem c -> case diffModuleName c of
            NoChange n -> n
            Change a b -> a ++ " / " ++ b

    render :: Elem ModuleDiff ModuleInterface -> HtmlT Program ()
    render = renderElem_ renderModuleInterface renderModuleDiff


renderModuleInterface :: ModuleInterface -> HtmlT Program ()
renderModuleInterface iface = do
    h2_ $ toHtml (moduleName iface)

    ul_ [ class_ "export-list" ] $
        mapM_ (li_ . renderExport) $
            compileModuleExports iface


renderModuleDiff :: ModuleDiff -> HtmlT Program ()
renderModuleDiff mdiff = do
    h2_ $ toHtml $ case diffModuleName mdiff of
        NoChange n -> n
        Change a b -> a ++ " / " ++ b

    ol_ [ class_ "export-list" ] $
        mapM_ (li_ . renderExportElem) $
            reverse $ diffModuleExports mdiff


renderPackageDiff :: PackageDiff -> HtmlT Program ()
renderPackageDiff = renderModuleDiffGroup . elemList . diffPkgExposedModules

renderExport :: Export -> HtmlT Program ()
renderExport (Named n e) = renderExportId n <> renderEntity e

renderExportElem :: ExportElem -> HtmlT Program ()
renderExportElem (Named n e) =
    renderExportId n <> renderElem_ renderEntity renderEntityDiff e

renderEntity :: Entity -> HtmlT Program ()
renderEntity e = do
    case e of
        LocalValue vd -> renderEntityType (vdType vd)
        LocalType td -> renderEntityKind (tdKind td)
        ReExport m ns ->
            ol_ $ do
                div_ [ class_ "namespace" ] $ showNamespace ns
                div_ [ class_ "module-name origin" ] $ toHtml m
  where
    showNamespace :: (Monad m) => Namespace -> HtmlT m ()
    showNamespace ns = case ns of
        Values -> "(value)"
        Types  -> "(type)"

renderEntityDiff :: EntityDiff -> HtmlT Program ()
renderEntityDiff = renderDiff_ renderEntity

renderEntityType :: Type -> HtmlT Program ()
renderEntityType t = do
    --qc <- lift getQualContext
    div_ [ class_ "type signature" ] $ " :: " <> renderType t

renderType :: (Monad m) => Type -> HtmlT m ()
renderType = renderTypeSig toHtml renderTypeCon . typeSig

renderTypeCon :: (Monad m) => TypeConLink -> HtmlT m ()
renderTypeCon q =
    abbr_ [ class_ "typecon", title_ t ] $ toHtml (rawName q)
  where
    t = Text.pack $ showQualName q

renderEntityKind :: Kind -> HtmlT Program ()
renderEntityKind k = do
    --qc <- lift getQualContext
    div_ [ class_ "kind signature" ] $ " :: " <> renderKind k

renderKind :: (Monad m) => Kind -> HtmlT m ()
renderKind k = case k of
    KindVar s -> toHtml s
    StarKind -> "*"
    HashKind -> "#"
    SuperKind -> "BOX"
    ConstraintKind -> "Constraint"
    PromotedType q -> toHtml (rawName q)
    FunKind a b -> renderKind a <> " -> " <> renderKind b

renderExportId :: (Monad m) => String -> HtmlT m ()
renderExportId = div_ [ class_ "export-id" ] . toHtml


renderElem ::
    (Monad m) =>
    (a -> HtmlT m a') ->     -- ^ added or removed element
    (c -> HtmlT m c') ->     -- ^ persistent element
    Elem c a -> HtmlT m (Elem c' a')
renderElem fa fc e = case e of
    Removed a -> div_ [ class_ "removed" ] (Removed <$> fa a)
    Added a   -> div_ [ class_ "added" ] (Added <$> fa a)
    Elem c    -> Elem <$> fc c

renderElem_ ::
    (Monad m) =>
    (a -> HtmlT m ()) ->
    (c -> HtmlT m ()) ->
    Elem c a -> HtmlT m ()
renderElem_ fa fc = void . renderElem fa fc


renderDiff_ :: (Monad m, Diff a c) => (a -> HtmlT m ()) -> c -> HtmlT m ()
renderDiff_ f = renderChange_ f . toChange


renderChange :: (Monad m) => (a -> HtmlT m b) -> Change a -> HtmlT m (Change b)
renderChange f (NoChange x) = div_ [ class_ "no-change" ] (NoChange <$> f x)
renderChange f (Change x y) =
    ol_ [ class_ "change" ] $ do
        a <- li_ [ class_ "old" ] (f x)
        b <- li_ [ class_ "new" ] (f y)
        pure $ Change a b

renderChange_ :: (Monad m) => (a -> HtmlT m ()) -> Change a -> HtmlT m ()
renderChange_ f = void . renderChange f
