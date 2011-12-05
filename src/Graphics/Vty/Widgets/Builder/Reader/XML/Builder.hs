module Graphics.Vty.Widgets.Builder.Reader.XML.Builder
    ( docFromXml
    , toSourceLocation
    )
where

import Control.Applicative
import Control.Monad (forM)

import Text.XML.HaXml.Types hiding (Reference)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Namespaces (printableName, localName)
import Text.XML.HaXml.Combinators hiding (tag)

import qualified Graphics.Vty.Widgets.Builder.AST as A
import Graphics.Vty.Widgets.Builder.Reader.XML.Types
import Graphics.Vty.Widgets.Builder.Reader.XML.Namespaces

toSourceLocation :: Posn -> A.SourceLocation
toSourceLocation p =
    A.SourceLocation { A.srcFile = posnFilename p
                     , A.srcLine = posnLine p
                     , A.srcColumn = posnColumn p
                     }

type NSURI = String
type ShortName = String

tag :: NSURI -> ShortName -> CFilter i
tag ns nam el@(CElem (Elem (QN actualNs n) _ _) _)
    | nsURI actualNs == ns && n == nam = [el]
tag _ _ _ = []

reqAttr :: Content Posn -> String -> XMLParse String
reqAttr (CElem (Elem eName attrs _) posn) nam =
    let err = ParseError msg posn
        msg = concat [ show $ localName eName
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
    elemNS e coreNS *>
    (A.Doc <$> parseInterfaces e
           <*> parseParams e
           <*> parseShared e
           <*> parseImports e)

parseInterfaces :: Content Posn -> XMLParse [A.Interface]
parseInterfaces =
    mapM parseInterface . childrenBy (tag coreNS "interface")
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
              expect coreNS "focusGroup" $ mapM parseFocusEntry . childrenBy elm

          parseFocusEntry =
              expect coreNS "entry" $
                         \e -> A.FocusReference
                               <$> reqAttr e "name"
                               <*> (pure $ toSourceLocation $ info e)

-- Looks for a 'params' child element of the specified element.  If
-- one is not found, returns empty list.
parseParams :: Content Posn -> XMLParse [A.Param]
parseParams e =
    concat <$> (mapM parseParams' $ childrenBy (tag coreNS "params") e)
        where
          parseParams' = mapM parseParam . childrenBy (tag coreNS "param")
          parseParam p = A.Param <$> reqAttr p "name"
                         <*> reqAttr p "type"
                         <*> pure (toSourceLocation $ info p)

-- Looks for a 'shared' child element of the specified element.  If
-- one is not found, returns empty list.
parseShared :: Content Posn -> XMLParse [(String, A.WidgetElement)]
parseShared e =
    concat <$> (mapM parseShared' $ childrenBy (tag coreNS "shared") e)
        where
          parseShared' sh =
              forM (childrenBy elm sh) $ \ch ->
                  (,)
                  <$> reqAttr ch "sharedId"
                  <*> parseWidgetElement ch

elemNS :: Content Posn -> NSURI -> XMLParse ()
elemNS (CElem (Elem (N _) _ _) posn) ns =
    ParseError ("Expected element in namespace " ++ show ns ++
                ", got unqualified element") posn
elemNS (CElem (Elem (QN actualNs _) _ _) posn) ns =
    if ns == nsURI actualNs
    then return ()
    else ParseError ("Expected element in namespace " ++ show ns ++
                     ", got element in namespace " ++
                     (show $ nsURI actualNs)) posn
elemNS c ns =
    ParseError ("Expected element in namespace " ++ show ns ++
                ", got non-element content") (info c)

parseWidgetElement :: Content Posn -> XMLParse A.WidgetElement
parseWidgetElement e@(CElem (Elem _ _ _) _) =
    elemNS e widgetNS *>
    (A.WidgetElement
          <$> (optional $ reqAttr e "id")
          <*> parseElement e)
parseWidgetElement c =
    ParseError ("Expected widget element in namespace " ++ (show widgetNS))
                   (info c)

attrValues :: Element Posn -> Posn -> XMLParse [(String, String)]
attrValues e@(Elem _ attrs _) posn =
    mapM (\k -> (,) k <$> reqAttr (CElem e posn) k) $ map (localName . fst) attrs

elemContents :: Element Posn -> XMLParse [A.ElementContent]
elemContents (Elem _ _ cs) = mapM parseElemContent cs
    where
      parseElemContent e@(CElem _ _) =
          (elemNS e dataNS *> (A.ChildElement <$> parseElement e))
          <|> (A.ChildWidgetLike <$> parseWidgetLike e)
      parseElemContent (CString _ s posn) = return $ A.Text s (toSourceLocation posn)
      parseElemContent c =
          ParseError "Content must be child element or string" (info c)

-- Note: no namespace requirement.
parseElement :: Content Posn -> XMLParse A.Element
parseElement (CElem e2@(Elem n _ _) posn) =
    (A.Element (localName n)
          <$> attrValues e2 posn
          <*> elemContents e2
          <*> (pure $ toSourceLocation posn))
parseElement c =
    ParseError "Expected element, got non-element content" (info c)

parseWidgetLike :: Content Posn -> XMLParse A.WidgetLike
parseWidgetLike e = (A.WidgetRef <$> parseWidgetReference e) <|>
                    (A.WidgetRef <$> parseParamReference e) <|>
                    (A.WidgetRef <$> parseSharedReference e) <|>
                    (A.Widget <$> parseWidgetElement e)

parseWidgetReference :: Content Posn -> XMLParse A.WidgetReference
parseWidgetReference =
    expect coreNS "wref" $ \r ->
        A.WidgetReference
             <$> reqAttr r "target"
             <*> (pure $ toSourceLocation $ info r)
             <*> (pure A.InterfaceWidgetRef)

parseParamReference :: Content Posn -> XMLParse A.WidgetReference
parseParamReference =
    expect coreNS "pref" $ \r ->
        A.WidgetReference
             <$> reqAttr r "target"
             <*> (pure $ toSourceLocation $ info r)
             <*> (pure A.ParameterRef)

parseSharedReference :: Content Posn -> XMLParse A.WidgetReference
parseSharedReference =
    expect coreNS "shref" $ \r ->
        A.WidgetReference
             <$> reqAttr r "target"
             <*> (pure $ toSourceLocation $ info r)
             <*> (pure A.SharedWidgetRef)

elemName :: Content Posn -> String
elemName (CElem (Elem nam _ _) _) = printableName nam
elemName _ = "(non-element content)"

expect :: NSURI
       -> String
       -> (Content Posn -> XMLParse a)
       -> Content Posn
       -> XMLParse a
expect ns nam f =
    \e -> let msg = concat [ "Element '"
                           , nam
                           , "' expected, element '"
                           , elemName e
                           , "' found instead"
                           ]
          in case tag ns nam e of
            [] -> ParseError msg (info e)
            [r] -> f r
            _ -> error "BUG: tag should have returned at most one result"

-- Looks for 'import' children element of the specified element.  If
-- one is not found, returns empty list.
parseImports :: Content Posn -> XMLParse [A.ModuleImport]
parseImports e =
    -- Find an "imports" child and map parseImport over its children
    mapM parseImport $ childrenBy (tag coreNS "import") e
        where
          parseImport i = A.ModuleImport <$> reqAttr i "name"