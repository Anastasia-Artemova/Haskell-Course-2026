{-# LANGUAGE OverloadedStrings #-}

module LeftRecursionSpec where

import Test.Hspec
import GrammarParser
import LeftRecursion

spec :: Spec
spec = describe "detectLeftRecursion" $ do

    -- No recursion 
    it "returns empty list for an empty grammar" $ do
        detectLeftRecursion (Grammar [])
            `shouldBe` []

    it "returns empty list for a simple terminal rule" $ do
        detectLeftRecursion (Grammar [Rule "A" (Term "a")])
            `shouldBe` []

    it "returns empty list for a non-recursive non-terminal" $ do
        detectLeftRecursion (Grammar [Rule "A" (NonTerm "B"), Rule "B" (Term "b")])
            `shouldBe` []

    it "returns empty list for a sequence starting with a terminal" $ do
        detectLeftRecursion (Grammar [Rule "A" (Seq [Term "a", NonTerm "A"])])
            `shouldBe` []

    it "returns empty list for right recursion" $ do
        detectLeftRecursion (Grammar [Rule "A" (Seq [Term "a", NonTerm "A"])])
            `shouldBe` []

    it "returns empty list for Many / Many1 (not direct left recursion)" $ do
        detectLeftRecursion (Grammar [Rule "A" (Many (NonTerm "A"))])
            `shouldBe` []

    -- Direct left recursion 
    it "detects direct left recursion via NonTerm" $ do
        detectLeftRecursion (Grammar [Rule "A" (NonTerm "A")])
            `shouldBe` ["A"]

    it "detects direct left recursion in a sequence" $ do
        detectLeftRecursion (Grammar [Rule "A" (Seq [NonTerm "A", Term "+", NonTerm "A"])])
            `shouldBe` ["A"]

    it "detects left recursion in one Alt branch" $ do
        detectLeftRecursion (Grammar [Rule "A" (Alt [Seq [NonTerm "A", Term "x"], Term "y"])])
            `shouldBe` ["A"]

    it "detects left recursion when ALL Alt branches are left-recursive" $ do
        detectLeftRecursion (Grammar [Rule "A" (Alt [Seq [NonTerm "A", Term "x"], Seq [NonTerm "A", Term "y"]])])
            `shouldBe` ["A"]

    it "detects left recursion in a deeply nested first position" $ do
        detectLeftRecursion (Grammar [Rule "A" (Seq [Seq [NonTerm "A", Term "x"], Term "y"])])
            `shouldBe` ["A"]

    -- Multiple rules 
    it "reports only the left-recursive rule among several" $ do
        detectLeftRecursion (Grammar
            [ Rule "A" (Term "a")
            , Rule "B" (Seq [NonTerm "B", Term "b"])
            , Rule "C" (Term "c") ])
            `shouldBe` ["B"]

    it "reports all left-recursive rules when multiple exist" $ do
        detectLeftRecursion (Grammar
            [ Rule "A" (Seq [NonTerm "A", Term "a"])
            , Rule "B" (Term "b")
            , Rule "C" (Seq [NonTerm "C", Term "c"]) ])
            `shouldBe` ["A", "C"]

    it "detects indirect left recursion (A -> B -> A)" $ do
        detectLeftRecursion (Grammar
            [ Rule "A" (NonTerm "B")
            , Rule "B" (NonTerm "A") ])
            `shouldBe` ["A", "B"]