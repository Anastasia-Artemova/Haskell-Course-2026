{-# LANGUAGE OverloadedStrings #-}

module GrammarParser where

import Control.Monad (void)
import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Grammar = Grammar [Rule] deriving (Show, Eq)

data Rule = Rule String Expr
  deriving (Show, Eq)

data Expr
  = Seq [Expr]
  | Alt [Expr]
  | Term String
  | NonTerm String
  | Many Expr
  | Many1 Expr
  | Optional Expr
  deriving (Show, Eq)

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme $ do
  first <- letterChar
  rest <- many alphaNumChar
  pure (first : rest)

terminal :: Parser Expr
terminal = Term <$> (char '"' *> manyTill L.charLiteral (char '"') <* sc)

nonTerminal :: Parser Expr
nonTerminal = NonTerm <$> identifier

parens :: Parser Expr
parens = between (symbol "(") (symbol ")") expr

factor :: Parser Expr
factor = do
  base <- choice
    [ terminal
    , nonTerminal
    , parens
    ]

  choice
    [ Many1 base <$ symbol "+"
    , Many base  <$ symbol "*"
    , Optional base <$ symbol "?"
    , pure base
    ]

sequenceExpr :: Parser Expr
sequenceExpr = do
  xs <- some factor
  pure $ case xs of
    [x] -> x
    _   -> Seq xs

expr :: Parser Expr
expr = do
  xs <- sequenceExpr `sepBy1` symbol "|"
  pure $ case xs of
    [x] -> x
    _   -> Alt xs

rule :: Parser Rule
rule = do
  name <- identifier
  void $ symbol "::="
  body <- expr
  void $ symbol ";"
  pure (Rule name body)

grammar :: Parser Grammar
grammar = sc *> (Grammar <$> many rule) <* eof

parseGrammar :: Text -> Either (ParseErrorBundle Text Void) Grammar
parseGrammar = parse grammar "grammar"
