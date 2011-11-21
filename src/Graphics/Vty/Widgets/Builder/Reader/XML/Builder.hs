module Graphics.Vty.Widgets.Builder.Reader.XML.Builder
    ( docFromXml
    , toSourceLocation
    )
where

import Control.Applicative

import Text.XML.HaXml.Types hiding (Reference)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators

import Graphics.Vty.Widgets.Builder.AST
import Graphics.Vty.Widgets.Builder.Reader.XML.Types

toSourceLocation :: Posn -> SourceLocation
toSourceLocation p =
    SourceLocation { srcFile = posnFilename p
                   , srcLine = posnLine p
                   , srcColumn = posnColumn p
                   }

shortName :: QName -> String
shortName (N s) = s
shortName (QN _ s) = s

reqAttr :: Content Posn -> String -> XMLParse String
reqAttr (CElem (Elem eName attrs _) posn) nam =
    let err = ParseError msg posn
        msg = concat [ show $ shortName eName
                     , " element requires "
                     , show nam
                     , " attribute"
                     ]

    in case lookup (N nam) attrs of
         Nothing -> err
         Just (AttValue []) -> err
         Just (AttValue vals) ->
             case vals of
               [] -> err
               (Left v:_) -> return v
               _ -> err
reqAttr _ _ =
    error "BUG: reqAttr called on non-element content!"

optAttr :: Content Posn -> String -> XMLParse (Maybe String)
optAttr e attrName = (Just <$> reqAttr e attrName) <|> (return Nothing)

docFromXml :: Content Posn -> XMLParse Doc
docFromXml e =
    Doc <$> parseInterfaces e
            <*> parseParams e
            <*> parseShared e
            <*> parseImports e

parseInterfaces :: Content Posn -> XMLParse [Interface]
parseInterfaces =
    mapM parseInterface . childrenBy (tag "interface")
        where
          parseInterface e =
              case childrenBy elm e of
                [ui, fg] ->
                    Interface <$> reqAttr e "name"
                                  <*> parseWidgetLike ui
                                  <*> parseFocusGroup fg
                                  <*> pure (toSourceLocation $ info e)
                _ -> ParseError "Element must have two child elements" (info e)

          parseFocusGroup e =
              mapM parseFocusEntry $ childrenBy (tag "entry") e

          parseFocusEntry e = reqAttr e "name"

-- Looks for a 'params' child element of the specified element.  If
-- one is not found, returns empty list.
parseParams :: Content Posn -> XMLParse [Param]
parseParams e =
    concat <$> (mapM parseParams' $ childrenBy (tag "params") e)
        where
          parseParams' = mapM parseParam . childrenBy (tag "param")
          parseParam p = Param <$> reqAttr p "name"
                         <*> reqAttr p "type"
                         <*> pure (toSourceLocation $ info p)

-- Looks for a 'shared' child element of the specified element.  If
-- one is not found, returns empty list.
parseShared :: Content Posn -> XMLParse [WidgetSpec]
parseShared e =
    concat <$> (mapM parseShared' $ childrenBy (tag "shared") e)
        where
          parseShared' = mapM parseWidgetSpec . childrenBy elm

parseWidgetSpec :: Content Posn -> XMLParse WidgetSpec
parseWidgetSpec e@(CElem elmt@(Elem nam _ _) posn) =
    WidgetSpec (shortName nam)
                   <$> optAttr e "id"
                   <*> attrValues elmt posn
                   <*> specContents elmt
                   <*> pure (toSourceLocation posn)
parseWidgetSpec c =
    ParseError "Content expected to be a child element" (info c)

attrValues :: Element Posn -> Posn -> XMLParse [(String, String)]
attrValues e@(Elem _ attrs _) posn =
    mapM (\k -> (,) k <$> reqAttr (CElem e posn) k) $ map (shortName . fst) attrs

specContents :: Element Posn -> XMLParse [WidgetSpecContent]
specContents (Elem _ _ cs) = mapM parseSpecContent cs
    where
      parseSpecContent e@(CElem _ _) = Child <$> parseWidgetLike e
      parseSpecContent (CString _ s posn) = return $ Text s (toSourceLocation posn)
      parseSpecContent c =
          ParseError "Content must be child element or string" (info c)

parseWidgetLike :: Content Posn -> XMLParse WidgetLike
parseWidgetLike e = (Ref <$> parseReference e) <|>
                    (Widget <$> parseWidgetSpec e)

parseReference :: Content Posn -> XMLParse Reference
parseReference e =
    Reference <$> reqAttr e "name" <*> (pure $ toSourceLocation $ info e)

-- Looks for 'import' children element of the specified element.  If
-- one is not found, returns empty list.
parseImports :: Content Posn -> XMLParse [ModuleImport]
parseImports e =
    -- Find an "imports" child and map parseImport over its children
    mapM parseImport $ childrenBy (tag "import") e
        where
          parseImport i = ModuleImport <$> reqAttr i "name"