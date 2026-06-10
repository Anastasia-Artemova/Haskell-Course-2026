module Builtins where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

matchBuiltin :: String -> Text -> Maybe (String, Text)

matchBuiltin "digit" input =
    case T.uncons input of
        Just (c, rest)
            | isDigit c -> Just ([c], rest)
        _ ->
            Nothing

matchBuiltin "letter" input =
    case T.uncons input of
        Just (c, rest)
            | isAlpha c -> Just ([c], rest)
        _ ->
            Nothing

matchBuiltin "space" input =
    case T.uncons input of
        Just (c, rest)
            | isSpace c -> Just ([c], rest)
        _ ->
            Nothing

matchBuiltin _ _ =
    Nothing