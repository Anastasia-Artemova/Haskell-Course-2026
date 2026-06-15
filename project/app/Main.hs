{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Data.Text as T
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Text.Megaparsec (errorBundlePretty)

import GrammarParser (parseGrammar)
import Interpreter (parseInput)
import PrettyPrinter (prettyPrint)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [file] -> do
      exists <- doesFileExist file

      if not exists
        then putStrLn ("File not found: " ++ file)
        else do
          contents <- readFile file

          case parseGrammar (T.pack contents) of
            Left err -> putStrLn ("Grammar parse error:\n" ++ errorBundlePretty err)

            Right grammar -> do
                putStrLn "Grammar parsed successfully."
                putStrLn "Provide input to parse: "
                input <- getLine

                case parseInput grammar (T.pack input) of
                    Left err -> putStrLn ("Input parse error: " ++ err)

                    Right tree -> do
                        putStrLn "Parse tree:"
                        putStrLn (prettyPrint tree)

    _ -> putStrLn "Usage: stack run <grammar-file>"