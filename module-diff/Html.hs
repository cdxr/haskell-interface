{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Monoid
import Data.Foldable
import Data.List ( intersperse )

import qualified Data.ByteString.Lazy as BS
import qualified Blaze.ByteString.Builder as Blaze

import Data.Text ( Text )
import qualified Data.Text as Text

import Text.Groom

import Data.Interface
import Data.Interface.Change
import Data.Interface.Change.View
import Data.Interface.Type.Render
import Data.Interface.Type.Diff

import Lucid

import Task
import Program
import ProgramArgs
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
        div_ [ class_ "package" ] $
            renderModuleGroup . toList $ pkgExposedModules iface

renderPackageDiffPage :: PackageDiff -> HtmlT Program ()
renderPackageDiffPage pdiff = do
    let title = showChange " / " showPackageId $ diffPkgId pdiff
    simplePage title $
        div_ [ class_ "package" ] $
            renderPackageDiff pdiff


createLink ::
    (Monad m, Monad n) =>
    Text ->                  -- ^ unique element id
    HtmlT m a ->             -- ^ hyperlink markup
    HtmlT n b ->             -- ^ linked content
    (HtmlT m a, HtmlT n b)
createLink uniqueId link content = (linkHtml, contentHtml)
  where
    linkHtml = a_ [ href_ (Text.pack "#" <> uniqueId) ] link
    contentHtml = with content [ id_ uniqueId ]


simpleLinkList ::
    (Monad m) =>
    (a -> Text) ->
    (a -> HtmlT m b) ->
    (a -> HtmlT m c) ->
    [a] -> HtmlT m ()
simpleLinkList mkId mkLink mkContent xs = do
    let (links, sections) =
            unzip [ createLink (mkId x) (mkLink x) (mkContent x) | x <- xs ]

    ol_ [ class_ "links" ] $ mapM_ (li_ [ class_ "link" ]) links

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


renderModuleGroup :: [ModuleInterface] -> HtmlT Program ()
renderModuleGroup = simpleLinkList makeId makeLink renderModuleInterface
  where
    makeId :: ModuleInterface -> Text
    makeId = moduleNameElemId . moduleName

    makeLink :: (Monad m) => ModuleInterface -> HtmlT m ()
    makeLink = toHtml . moduleName


-- | Format a `ModuleName` as a legal html element id.
moduleNameElemId :: ModuleName -> Text
moduleNameElemId = Text.pack . map replaceDot
  where
    replaceDot c | c == '.' = '-'
                 | otherwise = c


renderModuleDiffGroup :: [Elem ModuleDiff ModuleInterface] -> HtmlT Program ()
renderModuleDiffGroup = simpleLinkList makeId makeLinkText renderModuleElem
  where
    makeId :: Elem ModuleDiff ModuleInterface -> Text
    makeId e = case e of
        Removed m -> "removed-" <> moduleNameElemId (moduleName m)
        Added m   -> "added-" <> moduleNameElemId (moduleName m)
        Elem c    -> Text.pack $ new $ diffModuleName c

    makeLinkText :: (Monad m) => Elem ModuleDiff ModuleInterface -> HtmlT m ()
    makeLinkText e = case e of
        Removed m -> formatRemoved $ toHtml $ moduleName m
        Added m   -> formatRemoved $ toHtml $ moduleName m
        Elem c -> span_ [ class_ cls ] $
                    toHtml $ showChange " => " id (diffModuleName c)
          where
            cls | isElemChanged e = "change"
                | otherwise       = "no-change"


-- | Encode an invisible note in Html. The note can provide helpfull debugging
-- information when attached to an element that is inspected in the browser.
storeNote :: (Monad m) => String -> HtmlT m ()
storeNote = div_ [ class_ "note", style_ "display:none;" ] . toHtml


renderModuleInterface :: ModuleInterface -> HtmlT Program ()
renderModuleInterface = renderModuleDiff . noDiff

renderModuleDiff :: ModuleDiff -> HtmlT Program ()
renderModuleDiff = renderModuleElem . Elem

renderModuleElem :: Elem ModuleDiff ModuleInterface -> HtmlT Program ()
renderModuleElem moduleElem = do
    onlyChanges <- lift $ getArg onlyShowChanges

    let mdiff = joinElem moduleElem
        nameElem = diffModuleName mdiff
        exports = exportElems $ joinElem moduleElem
        visibleExports
            | onlyChanges = filter (isElemChanged . unName) exports
            | otherwise = exports

    div_ [ class_ "module" ] $ do
        h2_ $ toHtml $ showChange " => " id nameElem

        renderElemSummary moduleElem

        ol_ [ class_ "export-list" ] $
            mapM_ (li_ . renderExportElem nameElem) $ reverse visibleExports


renderElemSummary :: Elem ModuleDiff ModuleInterface -> HtmlT Program ()
renderElemSummary moduleElem =
    div_ [ class_ "elem-summary" ] $
        case moduleElem of
            Removed iface ->
                formatRemoved $ toHtml $
                    "removed module (" ++ countExports iface ++ " exports)"
            Added iface ->
                formatAdded $ toHtml $
                    "added module (" ++ countExports iface ++ " exports)"
            Elem mdiff ->
                mconcat $ intersperse ", " $
                    visibleStats (summarizeExports mdiff)
  where
    countExports = show . length . moduleExportList

    stats =
        [ ("removed",   removedCount)
        , ("added",     addedCount)
        , ("changed",   changedCount)
        , ("unchanged", unchangedCount)
        ]

    visibleStats summary =
        [ go s n | (s,f) <- stats
                 , let n = f summary
                 , n > 0 ]

    go :: (Monad m) => String -> Int -> HtmlT m ()
    go s n = span_ [ class_ (Text.pack s) ] $
                toHtml $ show n ++ " " ++ s


renderPackageDiff :: PackageDiff -> HtmlT Program ()
renderPackageDiff = renderModuleDiffGroup . elemList . diffPkgExposedModules

renderExportElem :: Change ModuleName -> ExportElem -> HtmlT Program ()
renderExportElem mc (Named n e) = do
    formatAs e $ dfn_ [class_ "export-id "] $
        abbr_ [title_ qualName] $ toHtml n

    renderElem_ renderEntity renderEntityDiff e
  where
    qualName = Text.pack $ showChange " => " showQual mc
    showQual m = showQualName $ Qual m n

    changeClass | isElemChanged e = "change"
                | otherwise       = "no-change"

renderEntity :: Entity -> HtmlT Program ()
renderEntity e =
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
renderEntityDiff ediff = case ediff of
    LocalValueDiff vd -> renderEntityTypeDiff (vdTypeDiff vd)
    LocalTypeDiff td  -> renderChange_ renderEntityKind (tdKindDiff td)
    EntityDiff c -> renderChange_ renderEntity c


renderEntityType :: Type -> HtmlT Program ()
renderEntityType = renderEntityTypeDiff . noDiff

renderEntityTypeDiff :: TypeDiff -> HtmlT Program ()
renderEntityTypeDiff tdiff = do
    let diffView :: DiffView (HtmlT Program ())
        diffView = renderTypeDiff' rc htmlTypeRender tdiff'

        tdiff' = extendTypeDiff tdiff

    case viewCombined diffView of
        Nothing -> renderChange_ asSignature (viewSeparate diffView)
        Just m -> asSignature m

    -- hide some notes in the HTML for debugging
    storeNote $ groom tdiff'
  where
    rc :: (Monad m) => RenderCombined (HtmlT m ())
    rc = Just . renderElem_ id (renderChange_ id . toChange)

    asSignature :: (Monad m) => HtmlT m () -> HtmlT m ()
    asSignature typeHtml =
        div_ [ class_ "type signature" ] $ " :: " <> typeHtml


htmlTypeRender :: (Monad m) => TypeRender (HtmlT m ())
htmlTypeRender =
    (toHtml <$> stringTypeRender)
        { renderCon = renderTypeCon }


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


renderElem ::
    (Monad m) =>
    (a -> HtmlT m a') ->     -- ^ added or removed element
    (c -> HtmlT m c') ->     -- ^ persistent element
    Elem c a -> HtmlT m (Elem c' a')
renderElem fa fc e = case e of
    Removed a -> formatRemoved $ Removed <$> fa a
    Added a   -> formatAdded $ Added <$> fa a
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


showChange ::
    String ->           -- ^ separator
    (a -> String) ->    -- ^ renderer
    Change a -> String
showChange s f c = case c of
    NoChange a -> f a
    Change a b -> f a ++ s ++ f b


formatAs :: (ToChange a c, Monad m) => Elem c a -> HtmlT m r -> HtmlT m r
formatAs e html = with html [class_ cls]
  where
    cls = case e of
        Removed{} -> "removed "
        Added{}   -> "added "
        Elem c | isChanged c -> "changed "
               | otherwise   -> "not-changed "

formatAdded :: (Monad m) => HtmlT m a -> HtmlT m a
formatAdded = span_ [ class_ "added" ]

formatRemoved :: (Monad m) => HtmlT m a -> HtmlT m a
formatRemoved = span_ [ class_ "removed" ]
