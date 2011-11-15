module Graphics.Vty.Widgets.Builder.Reader.XML
    ( xmlReader
    )
where

import Data.List (isSuffixOf)

import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn

import qualified Graphics.Vty.Widgets.Builder.AST as A
import Graphics.Vty.Widgets.Builder.Reader
import Graphics.Vty.Widgets.Builder.Reader.XML.Types
import Graphics.Vty.Widgets.Builder.Reader.XML.Builder
-- import Graphics.Vty.Widgets.Builder.Reader.XML.Validate

xmlReader :: DocumentReader
xmlReader = DocumentReader { checkFormat = checkXmlFormat
                           , readDoc = xmlReadDoc
                           }

parseAndValidate :: FilePath -> IO (Either [String] (Element Posn))
parseAndValidate inputXmlPath = do
  xmlContents <- readFile inputXmlPath
  case xmlParse' inputXmlPath xmlContents of
    Left e -> return $ Left ["Error parsing input XML "
                             ++ (show inputXmlPath) ++ ": " ++ e]
    Right (Document _ _ e _) -> return $ Right e
         -- result <- validate e
         -- case result of
         --   [] -> return $ Right e
         --   es -> return $ Left es

xmlReadDoc :: FilePath -> IO (Either [String] A.Doc)
xmlReadDoc path = do
  parsed <- parseAndValidate path
  case parsed of
    Left es -> return $ Left es
    Right e ->
        case buildDoc e of
          Error err -> return $ Left [err]
          Parsed doc -> return $ Right doc

checkXmlFormat :: FilePath -> IO Bool
checkXmlFormat = return . (".xml" `isSuffixOf`)