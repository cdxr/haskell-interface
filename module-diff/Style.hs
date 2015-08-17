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


highlightChange :: Css
highlightChange = fontWeight bold

mainStyle :: Css
mainStyle = do
    body ? do
        backgroundColor "#f0f0f0"
        fontFamily [] [monospace]

    ".elem-summary" & do
        padding nil (em 2) nil (em 2)
        star # ".changed" <? highlightChange

    ".removed" & do
        highlightChange
        color removalColor

    ".added" & do
        highlightChange
        color additionColor

    ".package" & do
        star # ".links" <? listStyleType none

    ".export-list" & do
        listStyleType none
        padding nil (em 1) nil (em 1)

        li <? do
            marginLeft nil
            lineHeight (pct 200)
            borderBottom solid 2 "#ddd"

    ".change" & highlightChange

    ".change" & do
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
    ".signature" & marginLeft (em 2)
        

additionColor :: Color
additionColor = "#006000" -- green

removalColor :: Color
removalColor = "#800000" -- red

noChangeColor :: Color
noChangeColor = "#848484"
