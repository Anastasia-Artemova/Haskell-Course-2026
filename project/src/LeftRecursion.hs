module LeftRecursion where

import GrammarParser

detectLeftRecursion :: Grammar -> [String]
detectLeftRecursion grammar@(Grammar rules) =
  [ name | Rule name body <- rules
         , checkForRecursion grammar name [] body ]

checkForRecursion :: Grammar -> String -> [String] -> Expr -> Bool
checkForRecursion grammar@(Grammar rules) target visited expr =
  case expr of
    NonTerm n
      | n == target -> True
      | n `elem` visited -> False
      | otherwise ->
          case lookupRule n grammar of
            Just body -> checkForRecursion grammar target (n : visited) body
            Nothing -> False

    Seq (first : _) ->
      checkForRecursion grammar target visited first

    Alt alternatives ->
      any (checkForRecursion grammar target visited) alternatives

    _ -> False

lookupRule :: String -> Grammar -> Maybe Expr
lookupRule name (Grammar rules) =
  case [expr | Rule ruleName expr <- rules, ruleName == name] of
    (x:_) -> Just x
    [] -> Nothing