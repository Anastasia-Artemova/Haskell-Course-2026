{-# LANGUAGE OverloadedStrings #-}

module InterpreterSpec where

import Test.Hspec
import Data.Either (isLeft)
import Interpreter
import GrammarParser

spec :: Spec
spec = describe "Interpreter" $ do

    -- Terminals
    describe "terminals" $ do
        it "parses a simple terminal" $ do
            let grammar = Grammar [Rule "Start" (Term "hello")]
            parseInput grammar "hello"
                `shouldBe`
                Right (PTNode "Start" [PTLeaf "hello"])

        it "fails on incorrect terminal" $ do
            let grammar = Grammar [Rule "Start" (Term "hello")]
            parseInput grammar "hi"
                `shouldSatisfy` isLeft

        it "fails on unconsumed trailing input" $ do
            let grammar = Grammar [Rule "Start" (Term "hi")]
            parseInput grammar "hi there"
                `shouldSatisfy` isLeft

        it "fails on empty input when terminal expected" $ do
            let grammar = Grammar [Rule "Start" (Term "x")]
            parseInput grammar ""
                `shouldSatisfy` isLeft

    -- Built-in: digit
    describe "builtin digit" $ do
        it "matches a single digit character" $ do
            let grammar = Grammar [Rule "D" (NonTerm "digit")]
            parseInput grammar "5"
                `shouldBe`
                Right (PTNode "D" [PTNode "digit" [PTLeaf "5"]])

        it "matches only the first digit, fails on trailing input" $ do
            let grammar = Grammar [Rule "D" (NonTerm "digit")]
            parseInput grammar "123"
                `shouldSatisfy` isLeft

        it "fails on a letter" $ do
            let grammar = Grammar [Rule "D" (NonTerm "digit")]
            parseInput grammar "a"
                `shouldSatisfy` isLeft

        it "fails on empty input" $ do
            let grammar = Grammar [Rule "D" (NonTerm "digit")]
            parseInput grammar ""
                `shouldSatisfy` isLeft

        it "parses multiple digits via Many1" $ do
            let grammar = Grammar [Rule "Digits" (Many1 (NonTerm "digit"))]
            parseInput grammar "42"
                `shouldBe`
                Right (PTNode "Digits"
                    [PTNode "many1"
                        [ PTNode "digit" [PTLeaf "4"]
                        , PTNode "digit" [PTLeaf "2"] ]])

        it "parses zero or more digits via Many on empty input" $ do
            let grammar = Grammar [Rule "Digits" (Many (NonTerm "digit"))]
            parseInput grammar ""
                `shouldBe`
                Right (PTNode "Digits" [PTNode "many" []])

    -- Built-in: letter
    describe "builtin letter" $ do
        it "matches a single lowercase letter" $ do
            let grammar = Grammar [Rule "L" (NonTerm "letter")]
            parseInput grammar "x"
                `shouldBe`
                Right (PTNode "L" [PTNode "letter" [PTLeaf "x"]])

        it "matches a single uppercase letter" $ do
            let grammar = Grammar [Rule "L" (NonTerm "letter")]
            parseInput grammar "Z"
                `shouldBe`
                Right (PTNode "L" [PTNode "letter" [PTLeaf "Z"]])

        it "fails on a digit" $ do
            let grammar = Grammar [Rule "L" (NonTerm "letter")]
            parseInput grammar "3"
                `shouldSatisfy` isLeft

        it "fails on empty input" $ do
            let grammar = Grammar [Rule "L" (NonTerm "letter")]
            parseInput grammar ""
                `shouldSatisfy` isLeft

        it "parses a word as Many1 letters" $ do
            let grammar = Grammar [Rule "Word" (Many1 (NonTerm "letter"))]
            parseInput grammar "hi"
                `shouldBe`
                Right (PTNode "Word"
                    [PTNode "many1"
                        [ PTNode "letter" [PTLeaf "h"]
                        , PTNode "letter" [PTLeaf "i"] ]])

    -- Built-in: space
    describe "builtin space" $ do
        it "matches a single space character" $ do
            let grammar = Grammar [Rule "S" (NonTerm "space")]
            parseInput grammar " "
                `shouldBe`
                Right (PTNode "S" [PTNode "space" [PTLeaf " "]])

        it "matches a tab character" $ do
            let grammar = Grammar [Rule "S" (NonTerm "space")]
            parseInput grammar "\t"
                `shouldBe`
                Right (PTNode "S" [PTNode "space" [PTLeaf "\t"]])

        it "fails on a non-space character" $ do
            let grammar = Grammar [Rule "S" (NonTerm "space")]
            parseInput grammar "a"
                `shouldSatisfy` isLeft

        it "parses optional whitespace" $ do
            let grammar = Grammar [Rule "WS" (Optional (NonTerm "space"))]
            parseInput grammar ""
                `shouldBe`
                Right (PTNode "WS" [PTNode "optional" []])

    -- Unknown built-in
    describe "unknown non-terminal" $ do
        it "fails on an unrecognised builtin name" $ do
            let grammar = Grammar [Rule "R" (NonTerm "number")]
            parseInput grammar "1"
                `shouldSatisfy` isLeft

        it "fails on a user non-terminal with no matching rule" $ do
            let grammar = Grammar [Rule "Start" (NonTerm "Missing")]
            parseInput grammar "x"
                `shouldSatisfy` isLeft

    -- Combining builtins
    describe "combining builtins" $ do
        it "parses letter followed by digit as a sequence" $ do
            let grammar = Grammar [Rule "R" (Seq [NonTerm "letter", NonTerm "digit"])]
            parseInput grammar "a1"
                `shouldBe`
                Right (PTNode "R"
                    [PTNode "seq"
                        [ PTNode "letter" [PTLeaf "a"]
                        , PTNode "digit"  [PTLeaf "1"] ]])

        it "fails when letter-then-digit sequence is given digit-then-letter" $ do
            let grammar = Grammar [Rule "R" (Seq [NonTerm "letter", NonTerm "digit"])]
            parseInput grammar "1a"
                `shouldSatisfy` isLeft

        it "parses letter-or-digit alternation with a letter" $ do
            let grammar = Grammar [Rule "AlNum" (Alt [NonTerm "letter", NonTerm "digit"])]
            parseInput grammar "b"
                `shouldBe`
                Right (PTNode "AlNum" [PTNode "alt" [PTNode "letter" [PTLeaf "b"]]])

        it "parses letter-or-digit alternation with a digit" $ do
            let grammar = Grammar [Rule "AlNum" (Alt [NonTerm "letter", NonTerm "digit"])]
            parseInput grammar "7"
                `shouldBe`
                Right (PTNode "AlNum" [PTNode "alt" [PTNode "digit" [PTLeaf "7"]]])

        it "parses a simple identifier: letter followed by Many digits" $ do
            let grammar = Grammar [Rule "Id" (Seq [NonTerm "letter", Many (NonTerm "digit")])]
            parseInput grammar "a3"
                `shouldBe`
                Right (PTNode "Id"
                    [PTNode "seq"
                        [ PTNode "letter" [PTLeaf "a"]
                        , PTNode "many" [PTNode "digit" [PTLeaf "3"]] ]])

        it "parses identifier with no trailing digits (Many matches zero)" $ do
            let grammar = Grammar [Rule "Id" (Seq [NonTerm "letter", Many (NonTerm "digit")])]
            parseInput grammar "a"
                `shouldBe`
                Right (PTNode "Id"
                    [PTNode "seq"
                        [ PTNode "letter" [PTLeaf "a"]
                        , PTNode "many" [] ]])

    -- User-defined non-terminals 
    describe "user-defined non-terminals" $ do
        it "resolves a rule that wraps a builtin" $ do
            let grammar = Grammar [ Rule "Start" (NonTerm "MyDigit")
                                  , Rule "MyDigit" (NonTerm "digit") ]
            parseInput grammar "9"
                `shouldBe`
                Right (PTNode "Start"
                    [PTNode "MyDigit"
                        [PTNode "digit" [PTLeaf "9"]]])

    -- Edge cases
    describe "edge cases" $ do
        it "fails on an empty grammar" $ do
            parseInput (Grammar []) "anything"
                `shouldSatisfy` isLeft
    
    describe "backtracking" $ do
        it "backtracks between alternatives inside a sequence" $ do
            let grammar = Grammar
                    [Rule "Start" (Seq [Alt [Term "a", Term "ab"], Term "c"])]
            parseInput grammar "abc"  `shouldSatisfy` either (const False) (const True)

        it "allows Many to stop early when needed" $ do
            let grammar = Grammar
                    [Rule "Start" (Seq [Many (Term "a"), Term "a"])]
            parseInput grammar "a"
            `shouldSatisfy` either (const False) (const True)

        it "allows Many1 to stop early when needed" $ do
            let grammar = Grammar
                    [Rule "Start" (Seq [Many1 (Term "a"), Term "a"])]
            parseInput grammar "aa"
            `shouldSatisfy` either (const False) (const True)

        it "allows Optional to choose empty when matching fails later" $ do
            let grammar = Grammar
                    [Rule "Start" (Seq [Optional (Term "a"), Term "a"])]
            parseInput grammar "a"
            `shouldSatisfy` either (const False) (const True)