module Graphics.Vty.Widgets.Builder.Reader.XML.Builder
    ( buildDoc
    )
where

import Control.Applicative

import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn

import Graphics.Vty.Widgets.Builder.AST
import Graphics.Vty.Widgets.Builder.Reader.XML.Types

-- noLoc :: SourceLocation
-- noLoc = SourceLocation "" 0 0

buildDoc :: Element Posn -> XMLParse Doc
buildDoc e =
    Doc <$> buildInterfaces e
            <*> buildParams e
            <*> buildShared e
            <*> buildImports e

buildInterfaces :: Element Posn -> XMLParse [Interface]
buildInterfaces _ = return []

buildParams :: Element Posn -> XMLParse [Param]
buildParams _ = return []

buildShared :: Element Posn -> XMLParse [WidgetSpec]
buildShared _ = return []

buildImports :: Element Posn -> XMLParse [ModuleImport]
buildImports _ = return []