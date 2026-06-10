{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified Data.Text as T

import GrammarParserSpec
import LeftRecursionSpec
import InterpreterSpec
import PrettyPrinterSpec
import EndToEndSpec
import PropertySpec

main :: IO ()
main = hspec $ do
    GrammarParserSpec.spec
    LeftRecursionSpec.spec
    InterpreterSpec.spec
    PrettyPrinterSpec.spec
    EndToEndSpec.spec
    PropertySpec.spec