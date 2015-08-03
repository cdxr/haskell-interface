{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Monoid
import Data.Foldable
import Data.List ( sortBy )

import qualified Data.ByteString.Lazy as BS
import qualified Blaze.ByteString.Builder as Blaze

import Data.Text ( Text )
import qualified Data.Text as Text

import Data.Interface
import Data.Interface.Change

import Lucid

import Task
import Program
import ProgramArgs
import Render
import Style


runTask :: ProgramResult -> Program ()
runTask r = do
    let html = case r of
            APackage p -> renderPackagePage p
            AModule tgt m -> renderModulePage tgt m
            AModuleDiff tgt0 tgt1 mdiff -> renderModuleDiffPage tgt0 tgt1 mdiff
            _ -> error "Html.runTask unimplemented for this target type"

    mfp <- getArg outputFile
    case mfp of
        Nothing -> stdoutHtml html
        Just fp -> writeHtml fp html


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
    let title = showPackageId iface
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
    simplePage title (renderModuleDiff t0 t1 mdiff)
  where
    Change name0 name1 = moduleName <$> toChange mdiff
    title | name0 == name1 = name1 ++ " (version comparison)"
          | otherwise = name0 ++ " / " ++ name1 ++ " (module comparison)"


renderModuleGroup :: [ModuleInterface] -> HtmlT Program ()
renderModuleGroup = 
    simpleLinkList makeId makeLinkText renderModuleInterface
  where
    makeId :: ModuleInterface -> Text
    makeId = Text.pack . map replaceDot . moduleName

    makeLinkText :: ModuleInterface -> String
    makeLinkText = moduleName

    replaceDot c
        | c == '.' = '-'
        | otherwise = c


renderModuleInterface :: ModuleInterface -> HtmlT Program ()
renderModuleInterface iface = do
    h2_ $ toHtml (moduleName iface)

    ul_ [ class_ "export-list" ] $
        mapM_ (li_ . renderExport) (compileModuleExports iface)


renderModuleDiff ::
    ModuleTarget -> ModuleTarget -> ModuleDiff -> HtmlT Program ()
renderModuleDiff tgt0 tgt1 mdiff = do
    ul_ $ do
        li_ [ class_ "module-source" ] $ toHtml $ moduleTargetString tgt0
        li_ [ class_ "module-source" ] $ toHtml $ moduleTargetString tgt1

    ul_ [ class_ "export-list" ] $
        mapM_ (li_ . renderElemExportDiff) $
            diffModuleExports mdiff
            --prepareExportDiffList $ diffModuleExports mdiff


-- | Sort and filter the list of export differences.
prepareExportDiffList :: [Elem ExportDiff Export] -> [Elem ExportDiff Export]
prepareExportDiffList = sortBy $ \a b -> case a of
    Added{}     -> LT
    Removed{}   -> LT
    Elem ediff | isChanged (exportDiffChange ediff) -> LT
    _ -> compare a b


renderExport :: Export -> HtmlT Program ()
renderExport e = do
    case e of
        LocalValue (Named n vd) ->
            renderExportId n <> renderType (vdType vd)
        LocalType (Named n td) ->
            renderExportId n <> renderKind (tdKind td)
        ReExport q -> do
            qc <- lift getQualContext
            renderExportId (resolveQual qc q) <> " (re-export)"

renderType :: Type -> HtmlT Program ()
renderType t = do
    qc <- lift getQualContext
    span_ [ class_ "type signature" ] $
        toHtml $ " :: " ++ renderToString 0 qc t

renderKind :: Kind -> HtmlT Program ()
renderKind k = do
    qc <- lift getQualContext
    span_ [ class_ "kind signature" ] $
        toHtml $ " :: " ++ renderToString 0 qc k


renderElemExportDiff :: Elem ExportDiff Export -> HtmlT Program ()
renderElemExportDiff = renderElemChange_ renderExport renderExportDiff

renderExportDiff :: ExportDiff -> HtmlT Program ()
renderExportDiff ediff =
    div_ [ class_ "export-diff" ] $
        case ediff of
            LocalValueDiff (Named n vd) ->
                div_ [ class_ "decl-value" ] $
                    renderExportId n <>
                        div_ [ class_ "decl-signature" ]
                            (renderDiff_ renderType (vdTypeDiff vd))
                -- ^ TODO: vdInfo field
            LocalTypeDiff (Named n td) ->
                div_ [ class_ "decl-type" ] $
                    renderExportId n <>
                        div_ [ class_ "decl-signature" ]
                            (renderDiff_ renderKind (tdKindDiff td))
                -- ^ TODO: tdInfo field
            _ -> undefined
            -- renderChange_ renderExport $ exportDiffChange ediff

renderExportId :: (Monad m) => String -> HtmlT m ()
renderExportId = span_ [ class_ "export-id" ] . toHtml

render :: (Render a) => a -> HtmlT Program ()
render a = do
    qc <- lift getQualContext
    p_ $ toHtml $ renderToString 2 qc a


renderElem :: (Monad m) =>
    (a -> HtmlT m b) ->     -- ^ removed element
    (a -> HtmlT m b) ->     -- ^ added element
    (c -> HtmlT m b) ->     -- ^ persistent element
    Elem c a -> HtmlT m b
renderElem rem add elem e = case e of
    Removed a -> div_ [ class_ "removed" ] (rem a)
    Added a   -> div_ [ class_ "added" ] (add a)
    Elem c    -> elem c


renderElemChange ::
    (Monad m) =>
    (a -> HtmlT m a') ->     -- ^ added or removed element
    (c -> HtmlT m c') ->     -- ^ persistent element
    Elem c a -> HtmlT m (Elem c' a')
renderElemChange f g =
    renderElem (fmap Removed . f) (fmap Added . f) (fmap Elem . g)

renderElemChange_ ::
    (Monad m) =>
    (a -> HtmlT m ()) ->     -- ^ added or removed element
    (c -> HtmlT m ()) ->     -- ^ persistent element
    Elem c a -> HtmlT m ()
renderElemChange_ f g = void . renderElemChange f g
    

renderDiff_ :: (Monad m, Diff a c) => (a -> HtmlT m ()) -> c -> HtmlT m ()
renderDiff_ f = renderChange_ f . toChange


renderChange :: (Monad m) => (a -> HtmlT m b) -> Change a -> HtmlT m (Change b)
renderChange f (NoChange x) = div_ [ class_ "no-change" ] (NoChange <$> f x)
renderChange f (Change x y) =
    ul_ [ class_ "change" ] $ do
        a <- li_ [ class_ "old" ] (f x)
        b <- li_ [ class_ "new" ] (f y)
        pure $ Change a b

renderChange_ :: (Monad m) => (a -> HtmlT m ()) -> Change a -> HtmlT m ()
renderChange_ f = void . renderChange f
