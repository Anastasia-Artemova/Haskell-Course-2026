{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Data.Text (Text)
import qualified Data.Text as T
import GrammarParser
import Builtins
import LeftRecursion (detectLeftRecursion)

data ParseTree = 
    PTNode String [ParseTree]  -- Non-terminal with children
  | PTLeaf String              -- Terminal
    deriving (Show, Eq)

parseInput :: Grammar -> Text -> Either String ParseTree
parseInput grammar input =
  case detectLeftRecursion grammar of
    (r:rs) ->
      Left ("Left recursion detected in rules: " ++ unwords (r:rs))
    [] ->
      case grammar of
        Grammar [] ->
          Left "Grammar is empty"

        Grammar (Rule startName startExpr : _) ->
          case [ PTNode startName [tree]
               | (tree, rest) <- parseExprAll grammar startExpr input
               , T.null rest
               ] of
            (tree : _) -> Right tree
            [] -> Left "Input does not match grammar"


parseExprAll :: Grammar -> Expr -> Text -> [(ParseTree, Text)]
parseExprAll grammar expr input =
  case expr of
    Term s ->
      let t = T.pack s
      in if t `T.isPrefixOf` input
           then [(PTLeaf s, T.drop (T.length t) input)]
           else []

    NonTerm name ->
      case matchBuiltin name input of
        Just (matched, rest) ->
          [(PTNode name [PTLeaf matched], rest)]

        Nothing ->
          case lookupRule name grammar of
            Nothing -> []
            Just body ->
              [ (PTNode name [tree], rest)
              | (tree, rest) <- parseExprAll grammar body input
              ]

    Seq xs ->
      parseSeqAll grammar xs input

    Alt xs ->
      [ (PTNode "alt" [tree], rest)
      | e <- xs
      , (tree, rest) <- parseExprAll grammar e input
      ]

    Many e ->
      parseManyAll grammar e input

    Many1 e ->
      [ (PTNode "many1" (firstTree : moreTrees), rest2)
      | (firstTree, rest1) <- parseExprAll grammar e input
      , rest1 /= input
      , (PTNode _ moreTrees, rest2) <- parseManyAll grammar e rest1
      ]

    Optional e ->
      parseExprAll grammar e input ++ [(PTNode "optional" [], input)]

lookupRule :: String -> Grammar -> Maybe Expr
lookupRule name (Grammar rules) =
  case [expr | Rule ruleName expr <- rules, ruleName == name] of
    (x:_) -> Just x
    []    -> Nothing

parseSeqAll :: Grammar -> [Expr] -> Text -> [(ParseTree, Text)]
parseSeqAll grammar exprs input =
  [ (PTNode "seq" trees, rest)
  | (trees, rest) <- go exprs input
  ]
  where
    go [] rest = [([], rest)]

    go (e:es) currentInput =
      [ (tree : trees, finalRest)
      | (tree, rest) <- parseExprAll grammar e currentInput
      , (trees, finalRest) <- go es rest
      ]

parseManyAll :: Grammar -> Expr -> Text -> [(ParseTree, Text)]
parseManyAll grammar e input =
  zeroMatch ++ moreMatches
  where
    zeroMatch =
      [(PTNode "many" [], input)]

    moreMatches =
      [ (PTNode "many" (tree : trees), finalRest)
      | (tree, rest) <- parseExprAll grammar e input
      , rest /= input
      , (PTNode _ trees, finalRest) <- parseManyAll grammar e rest
      ]
