{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Arrow ( (&&&) )
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
            -- PrintModule m -> renderModulePage m
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


renderPackagePage :: PackageInterface -> HtmlT Program ()
renderPackagePage iface = doctypehtml_ $ do
    let title = fromString (showPackageId iface)
    head_ $ title_ title
    body_ $ do
        h1_ [ class_ "title" ] title
        renderModuleGroup . toList $ pkgExposedModules iface


renderModuleGroup :: [ModuleInterface] -> HtmlT Program ()
renderModuleGroup ifaces = do
    let linkIfaces = map (makeIfaceLink &&& id) ifaces

    div_ [ class_ "links" ] $
        ul_ $ forM_ linkIfaces $ \(linkText, m) ->
            li_ $ a_ [ href_ (Text.pack "#" <> linkText) ] $
                fromString $ moduleName m
            
    forM_ linkIfaces $ \(linkText, m) ->
        renderModuleInterface m `with` [ id_ linkText ]
  where
    makeIfaceLink :: ModuleInterface -> Text
    makeIfaceLink = Text.pack . map replaceDot . moduleName

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
