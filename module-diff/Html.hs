{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Monoid
import Data.Foldable
import Data.String ( fromString )

import qualified Data.ByteString.Lazy as BS
import qualified Blaze.ByteString.Builder as Blaze

import Data.Text ( Text )
import qualified Data.Text as Text

import Data.Interface

import Lucid

import Task
import Program
import ProgramArgs
import Render


runTask :: LoadedTask -> Program ()
runTask t = do
    let html = case t of
            PrintPackage p -> renderPackagePage p
            PrintModule (tgt,m) -> renderModulePage tgt m
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
    let title = fromString titleString
    head_ $ title_ title
    body_ $ do
        h1_ [ class_ "title" ] title
        html


renderPackagePage :: PackageInterface -> HtmlT Program ()
renderPackagePage iface = do
    let title = fromString (showPackageId iface)
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
    createLink (mkId a) (fromString $ mkText a) (mkContent a)


simpleLinkList :: (Monad m) =>
    (a -> Text) ->
    (a -> String) ->
    (a -> HtmlT m b) ->
    [a] -> HtmlT m ()
simpleLinkList mkId mkText mkContent xs = do
    let (links, sections) =
            unzip $ map (createTextLink mkId mkText mkContent) xs

    div_ [ class_ "links" ] $ ul_ $ mapM_ li_ links
        
    div_ $ sequence_ sections


renderModulePage :: ModuleTarget -> ModuleInterface -> HtmlT Program ()
renderModulePage target = simplePage target . renderModuleInterface


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
    h2_ $ fromString (moduleName iface)

    mapM_ renderExport (compileModuleExports iface)


renderExport :: Export -> HtmlT Program ()
renderExport e = do
    qc <- lift getQualContext
    let showR :: (Render a) => a -> String
        showR = renderToString 0 qc
    p_ $ case e of
        LocalValue (Named n vd) ->
            fromString $ n ++ " :: " ++ showR (vdType vd)
        LocalType (Named n td) ->
            fromString $ n ++ " :: " ++ showR (tdKind td)
        ReExport q ->
            fromString $ resolveQual qc q ++ " (re-export)"


render :: (Render a) => a -> HtmlT Program ()
render a = do
    qc <- lift getQualContext
    p_ $ fromString $ renderToString 2 qc a
