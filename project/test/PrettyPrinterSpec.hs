{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinterSpec where

import Test.Hspec
import Interpreter
import PrettyPrinter

spec :: Spec
spec = describe "prettyPrint" $ do

    it "prints a single leaf" $ do
        prettyPrint (PTLeaf "hello")
            `shouldBe` "\"hello\"\n"

    it "prints a node with no children" $ do
        prettyPrint (PTNode "Empty" [])
            `shouldBe` "Empty\n"

    it "prints a node with one leaf child" $ do
        prettyPrint (PTNode "Start" [PTLeaf "hi"])
            `shouldBe` unlines
                [ "Start"
                , "  \"hi\"" ]

    it "prints a node with multiple leaf children" $ do
        prettyPrint (PTNode "Seq" [PTLeaf "a", PTLeaf "b", PTLeaf "c"])
            `shouldBe` unlines
                [ "Seq"
                , "  \"a\""
                , "  \"b\""
                , "  \"c\"" ]

    it "indents nested nodes correctly" $ do
        prettyPrint (PTNode "Root" [PTNode "Child" [PTLeaf "x"]])
            `shouldBe` unlines
                [ "Root"
                , "  Child"
                , "    \"x\"" ]

    it "indents three levels deep" $ do
        prettyPrint (PTNode "A" [PTNode "B" [PTNode "C" [PTLeaf "d"]]])
            `shouldBe` unlines
                [ "A"
                , "  B"
                , "    C"
                , "      \"d\"" ]

    it "handles siblings at the same level independently" $ do
        prettyPrint (PTNode "Root"
            [ PTNode "Left"  [PTLeaf "a"]
            , PTNode "Right" [PTLeaf "b"] ])
            `shouldBe` unlines
                [ "Root"
                , "  Left"
                , "    \"a\""
                , "  Right"
                , "    \"b\"" ]

    it "uses show for leaves, so special characters are escaped" $ do
        prettyPrint (PTLeaf "say \"hi\"")
            `shouldBe` "\"say \\\"hi\\\"\"\n"