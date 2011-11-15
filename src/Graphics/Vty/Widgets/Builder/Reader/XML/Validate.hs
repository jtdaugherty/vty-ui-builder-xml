module Graphics.Vty.Widgets.Builder.Reader.XML.Validate
    ()
where

-- import Text.XML.HaXml.Types
-- import Text.XML.HaXml.Posn

-- import Graphics.Vty.Widgets.Builder.Reader.XML.Types

-- validate :: Element Posn -> IO [String]
-- validate e hs = do
--   let initState = ValidationState [] validators
--       elements = map contentElem $ (multi elm) (CElem e noPos)
--   (_, st) <- runStateT (mapM validateSingle elements) initState
--   return $ errors st

-- validateSingle :: Element Posn -> ValidateM ()
-- validateSingle e = do
--   vs <- gets theValidators
--   case lookup (elemName e) vs of
--     Nothing -> return ()
--     Just f -> f e

-- putError :: Element Posn -> String -> ValidateM ()
-- putError e msg = do
--   let fullMsg = elemName e ++ ": " ++ msg

--   es <- gets errors
--   modify $ \st -> st { errors = es ++ [fullMsg] }

-- validators :: [(String, ElementValidator)]
-- validators =
--     [
--     ]