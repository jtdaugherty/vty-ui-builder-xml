module Graphics.Vty.Widgets.Builder.Reader.XML.Builder
    ( buildDoc
    , noLoc
    )
where

import Control.Applicative

import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators

import Graphics.Vty.Widgets.Builder.AST
import Graphics.Vty.Widgets.Builder.Reader.XML.Types

noLoc :: SourceLocation
noLoc = SourceLocation "" 0 0

getAttribute :: Content Posn -> String -> XMLParse (Maybe String)
getAttribute (CElem (Elem _ attrs _) _) nam =
    case lookup (N nam) attrs of
      Nothing -> return Nothing
      Just (AttValue []) -> return Nothing
      Just (AttValue vals) ->
          case vals of
            [] -> return Nothing
            (Left v:_) -> return $ Just v
            _ -> return Nothing
getAttribute _ _ = return Nothing

buildDoc :: Content Posn -> XMLParse Doc
buildDoc e =
    Doc <$> buildInterfaces e
            <*> buildParams e
            <*> buildShared e
            <*> buildImports e

buildInterfaces :: Content Posn -> XMLParse [Interface]
buildInterfaces _ = return []

-- Looks for a 'params' child element of the specified element.  If
-- one is not found, returns empty list.
buildParams :: Content Posn -> XMLParse [Param]
buildParams _ = return []

-- Looks for a 'shared' child element of the specified element.  If
-- one is not found, returns empty list.
buildShared :: Content Posn -> XMLParse [WidgetSpec]
buildShared _ = return []

-- Looks for 'import' children element of the specified element.  If
-- one is not found, returns empty list.
buildImports :: Content Posn -> XMLParse [ModuleImport]
buildImports e =
    -- Find an "imports" child and map buildImport over its children
    mapM buildImport $ (tag "import" /> tag "import") e

buildImport :: Content Posn -> XMLParse ModuleImport
buildImport e = do
  val <- getAttribute e "name"

  case val of
    Nothing -> fail "import element requires 'name' attribute"
    Just nam -> return $ ModuleImport nam