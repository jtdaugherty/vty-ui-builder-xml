{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Main where

import System.Exit

import Graphics.Vty.Widgets.All
import Graphics.Vty hiding (Button)
import Data.Monoid

import Graphics.Vty.Widgets.Builder.Reader.XML (xmlUi)

[xmlUi|
<?xml version="1.0" encoding="UTF-8" ?>
<collection xmlns:c="http://codevine.org/vty-ui-builder/xmlns/core/1.0"
            xmlns:d="http://codevine.org/vty-ui-builder/xmlns/data/1.0"
            xmlns:w="http://codevine.org/vty-ui-builder/xmlns/widget/1.0"
            xmlns="http://codevine.org/vty-ui-builder/xmlns/core/1.0">
  <interface name="intro">
    <w:vBox>
      <w:fText>This is an example of an embedded interface.</w:fText>
      <w:hBorder/>
      <w:fText>
        Here is some text!
      </w:fText>
    </w:vBox>
  </interface>

  <interface name="ui2">
    <w:wrap>
      <w:fText>This is a <d:attr fg="green">second</d:attr> interface.</w:fText>
    </w:wrap>
  </interface>
</collection>
|]

main :: IO ()
main = do
  (c, values) <- buildCollection

  fg_intro values `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KASCII 'n' -> switchTo_ui2 values >> return True
        _ -> return False

  fg_ui2 values `onKeyPressed` \_ k _ ->
      case k of
        KASCII 'q' -> exitSuccess
        KASCII 'n' -> switchTo_intro values >> return True
        _ -> return False

  runUi c defaultContext
