{-# LANGUAGE OverloadedStrings #-}

{-|
This module defines styles for the HTML renderer. In the future, this will
likely be rendered statically as a CSS file.
-}

module Style ( mainStyleText ) where

import Data.Text.Lazy ( Text )

import Clay


mainStyleText :: Text
mainStyleText = render mainStyle


mainStyle :: Css
mainStyle = do
    body ? do
        backgroundColor "#f0f0f0"
        fontFamily [] [monospace]

    byClass "removed" & color removalColor
    byClass "added" & color additionColor
    --byClass "no-change" & color noChangeColor

    byClass "export-id" & fontWeight bold

    byClass "export-list" & do
        listStyleType none
        padding nil (em 1) nil (em 1)

        li <? do
            marginLeft nil
            lineHeight (pct 200)
            borderBottom solid 2 "#eee"

    byClass "change" & do
        listStyleType none
        paddingLeft nil

        li <? do
            marginLeft nil
--            lineHeight (180 :: Size Rel)

        li # byClass "old" <? do
            color removalColor
        li # byClass "new" <? do
            color additionColor

    -- TODO: all "decl" properties should be indented
    byClass "signature" & marginLeft (em 2)
        

additionColor :: Color
additionColor = "#006000" -- green

removalColor :: Color
removalColor = "#800000" -- red

noChangeColor :: Color
noChangeColor = "#848484"
