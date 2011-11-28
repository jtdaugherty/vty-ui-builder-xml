module Graphics.Vty.Widgets.Builder.Reader.XML.Reader
    ( xmlReader
    )
where

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Combinators (tag)

import qualified Graphics.Vty.Widgets.Builder.AST as A
import Graphics.Vty.Widgets.Builder.Reader
import Graphics.Vty.Widgets.Builder.Reader.XML.Types
import Graphics.Vty.Widgets.Builder.Reader.XML.Builder

xmlReader :: DocumentReader
xmlReader = DocumentReader { readDoc = xmlReadDoc
                           }

parseAndValidate :: FilePath -> String -> IO (Either [(String, A.SourceLocation)] (Element Posn))
parseAndValidate inputXmlPath xmlContents = do
  case xmlParse' inputXmlPath xmlContents of
    Left e -> return $ Left [ ("Error parsing input XML "
                               ++ (show inputXmlPath) ++ ": " ++ e
                              , A.noLoc)
                            ]
    Right d -> do
      let (Document _ _ e _) = resolveAllNames qualify d
      return $ Right e

xmlReadDoc :: FilePath -> String -> IO (Either [(String, A.SourceLocation)] A.Doc)
xmlReadDoc path src = do
  parsed <- parseAndValidate path src
  case parsed of
    Left es -> return $ Left es
    Right e ->
        case tag "collection" (CElem e noPos) of
          [] -> return $ Left [("Root element expected to be 'collection'", A.noLoc)]
          [e'] -> case docFromXml e' of
                   ParseError err p -> return $ Left [(err, toSourceLocation p)]
                   Parsed doc -> return $ Right doc
          _ -> error "cannot happen"
