module Graphics.Vty.Widgets.Builder.Reader.XML.Types
    ( ValidationState(..)
    , XMLParse(..)
    , ValidateM
    , ElementValidator
    )
where

import Control.Applicative
import Control.Monad.State

import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn

data ValidationState =
    ValidationState { errors :: [String]
                    , theValidators :: [(String, ElementValidator)]
                    }

type ValidateM a = State ValidationState a

type ElementValidator = Element Posn -> ValidateM ()

data XMLParse a = Parsed a
                | Error String

instance Monad XMLParse where
    (Parsed val) >>= f = f val
    (Error s) >>= _ = Error s
    (Parsed _) >> (Parsed val) = Parsed val
    (Parsed _) >> (Error s) = Error s
    (Error s) >> _ = Error s
    fail = Error
    return = Parsed

instance Functor XMLParse where
    fmap _ (Error s) = Error s
    fmap f (Parsed a) = Parsed (f a)

instance Applicative XMLParse where
    pure = Parsed
    (Parsed f) <*> (Parsed val) = Parsed $ f val
    (Error s) <*> _ = Error s
    _ <*> (Error s) = Error s
