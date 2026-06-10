module PrettyPrinter where

import Interpreter

prettyPrint :: ParseTree -> String
prettyPrint = go 0
  where
    go :: Int -> ParseTree -> String
    go level (PTLeaf s) =
      replicate (2 * level) ' ' ++ show s ++ "\n"

    go level (PTNode name children) =
      replicate (2 * level) ' ' ++ name ++ "\n"
        ++ concatMap (go (level + 1)) children