module Graphics.Vty.Widgets.Builder.Reader.XML.Builder
    ( docFromXml
    , toSourceLocation
    )
where

import Control.Applicative

import Text.XML.HaXml.Types hiding (Reference)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Namespaces (printableName)
import Text.XML.HaXml.Combinators

import qualified Graphics.Vty.Widgets.Builder.AST as A
import Graphics.Vty.Widgets.Builder.Reader.XML.Types

toSourceLocation :: Posn -> A.SourceLocation
toSourceLocation p =
    A.SourceLocation { A.srcFile = posnFilename p
                     , A.srcLine = posnLine p
                     , A.srcColumn = posnColumn p
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

docFromXml :: Content Posn -> XMLParse A.Doc
docFromXml e =
    A.Doc <$> parseInterfaces e
         <*> parseParams e
         <*> parseShared e
         <*> parseImports e

parseInterfaces :: Content Posn -> XMLParse [A.Interface]
parseInterfaces =
    mapM parseInterface . childrenBy (tag "interface")
        where
          parseInterface e =
              case childrenBy elm e of
                [ui, fg] -> mkInterface e ui $ Just fg
                [ui] -> mkInterface e ui Nothing
                _ -> ParseError "Element must have exactly one or two child elements" (info e)

          mkInterface :: Content Posn -> Content Posn
                      -> Maybe (Content Posn) -> XMLParse A.Interface
          mkInterface e ui fg =
              A.Interface <$> reqAttr e "name"
                   <*> parseWidgetLike ui
                   <*> maybe (pure []) parseFocusGroup fg
                   <*> pure (toSourceLocation $ info e)

          parseFocusGroup =
              expect "focusGroup" $ mapM parseFocusEntry . childrenBy elm

          parseFocusEntry =
              expect "entry" $ \e -> reqAttr e "name"

-- Looks for a 'params' child element of the specified element.  If
-- one is not found, returns empty list.
parseParams :: Content Posn -> XMLParse [A.Param]
parseParams e =
    concat <$> (mapM parseParams' $ childrenBy (tag "params") e)
        where
          parseParams' = mapM parseParam . childrenBy (tag "param")
          parseParam p = A.Param <$> reqAttr p "name"
                         <*> reqAttr p "type"
                         <*> pure (toSourceLocation $ info p)

-- Looks for a 'shared' child element of the specified element.  If
-- one is not found, returns empty list.
parseShared :: Content Posn -> XMLParse [A.WidgetSpec]
parseShared e =
    concat <$> (mapM parseShared' $ childrenBy (tag "shared") e)
        where
          parseShared' = mapM parseWidgetSpec . childrenBy elm

parseWidgetSpec :: Content Posn -> XMLParse A.WidgetSpec
parseWidgetSpec e@(CElem elmt@(Elem nam _ _) posn) =
    A.WidgetSpec (shortName nam)
         <$> (optional $ reqAttr e "id")
         <*> attrValues elmt posn
         <*> specContents elmt
         <*> pure (toSourceLocation posn)
parseWidgetSpec c =
    ParseError "Content expected to be a child element" (info c)

attrValues :: Element Posn -> Posn -> XMLParse [(String, String)]
attrValues e@(Elem _ attrs _) posn =
    mapM (\k -> (,) k <$> reqAttr (CElem e posn) k) $ map (shortName . fst) attrs

specContents :: Element Posn -> XMLParse [A.WidgetSpecContent]
specContents (Elem _ _ cs) = mapM parseSpecContent cs
    where
      parseSpecContent e@(CElem _ _) = A.ChildWidgetLike <$> parseWidgetLike e
      parseSpecContent (CString _ s posn) = return $ A.Text s (toSourceLocation posn)
      parseSpecContent c =
          ParseError "Content must be child element or string" (info c)

parseWidgetLike :: Content Posn -> XMLParse A.WidgetLike
parseWidgetLike e = (A.Ref <$> parseReference e) <|>
                    (A.Widget <$> parseWidgetSpec e)

parseReference :: Content Posn -> XMLParse A.Reference
parseReference =
    expect "ref" $ \r ->
        A.Reference <$> reqAttr r "target" <*> (pure $ toSourceLocation $ info r)

elemName :: Content Posn -> String
elemName (CElem (Elem nam _ _) _) = printableName nam
elemName _ = "(non-element content)"

expect :: String -> (Content Posn -> XMLParse a) -> Content Posn -> XMLParse a
expect nam f =
    \e -> let msg = concat [ "Element '"
                           , nam
                           , "' expected, element '"
                           , elemName e
                           , "' found instead"
                           ]
          in case tag nam e of
            [] -> ParseError msg (info e)
            [r] -> f r
            _ -> error "BUG: tag should have returned at most one result"

-- Looks for 'import' children element of the specified element.  If
-- one is not found, returns empty list.
parseImports :: Content Posn -> XMLParse [A.ModuleImport]
parseImports e =
    -- Find an "imports" child and map parseImport over its children
    mapM parseImport $ childrenBy (tag "import") e
        where
          parseImport i = A.ModuleImport <$> reqAttr i "name"