module Graphics.Vty.Widgets.Builder.Reader.XML.TH
    ( xmlUi
    , xmlUiFile
    )
where

import Control.Monad

import Language.Haskell.Exts.Syntax
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Language.Haskell.Convert

import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.Reader.XML.Reader
import Graphics.Vty.Widgets.Builder.Reader
import Graphics.Vty.Widgets.Builder.Handlers
import Graphics.Vty.Widgets.Builder.Config

exprMsg :: String
exprMsg = "Expression quasi-quotation of user interface documents is unsupported"

patMsg :: String
patMsg = "Pattern quasi-quotation of user interface documents is unsupported"

typeMsg :: String
typeMsg = "Type quasi-quotation of user interface documents is unsupported"

xmlUi :: QuasiQuoter
xmlUi =
    QuasiQuoter { quoteExp = const $ fail exprMsg
                , quotePat = const $ fail patMsg
                , quoteType = const $ fail typeMsg
                , quoteDec = doQuoteDec
                }

xmlUiFile :: QuasiQuoter
xmlUiFile = quoteFile xmlUi

doQuoteDec :: String -> Q [Dec]
doQuoteDec src = do
  result <- runIO $ readDoc xmlReader "fixme" src
  case result of
    Left es -> do
                forM_ es $ \(msg, _) -> report True $ msg
                fail "Could not parse input document due to previous errors"
    Right doc -> do
                -- This may be the default config already, but this
                -- way we're explicit in case the defaults ever
                -- change, since this is what is necessary for the
                -- splice to work.
                let config = defaultConfig { generateMain = False
                                           , generateInterfaceType = True
                                           , generateInterfaceBuilder = True
                                           }
                modResult <- runIO $ generateSourceForDocument config doc coreSpecHandlers
                case modResult of
                  Left es -> do
                    forM_ es $ report True . show
                    fail "Could not generate module source due to previous errors"
                  Right (Module _ _ _ _ _ _ decls) -> return $ convert decls