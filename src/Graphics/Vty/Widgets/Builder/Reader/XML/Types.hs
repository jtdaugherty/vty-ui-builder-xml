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
                | ParseError String Posn

instance Monad XMLParse where
    (Parsed val) >>= f = f val
    (ParseError s p) >>= _ = ParseError s p
    (Parsed _) >> (Parsed val) = Parsed val
    (Parsed _) >> (ParseError s p) = ParseError s p
    (ParseError s p) >> _ = ParseError s p
    fail = flip ParseError noPos
    return = Parsed

instance Functor XMLParse where
    fmap _ (ParseError s p) = ParseError s p
    fmap f (Parsed a) = Parsed (f a)

instance Applicative XMLParse where
    pure = Parsed
    (Parsed f) <*> (Parsed val) = Parsed $ f val
    (ParseError s p) <*> _ = ParseError s p
    _ <*> (ParseError s p) = ParseError s p

instance Alternative XMLParse where
    empty = ParseError "Nothing parsed" noPos
    (ParseError _ _) <|> b = b
    (Parsed a) <|> _ = Parsed a