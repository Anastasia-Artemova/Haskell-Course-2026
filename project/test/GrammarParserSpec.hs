{-# LANGUAGE OverloadedStrings #-}

module GrammarParserSpec where

import Test.Hspec
import Data.Either (isLeft)
import GrammarParser

spec :: Spec
spec = describe "GrammarParser" $ do

    -- Terminals & non-terminals
    it "parses a terminal rule" $ do
        parseGrammar "Greeting ::= \"hello\" ;"
            `shouldBe`
            Right (Grammar [Rule "Greeting" (Term "hello")])

    it "parses a non-terminal rule" $ do
        parseGrammar "Start ::= expr ;"
            `shouldBe`
            Right (Grammar [Rule "Start" (NonTerm "expr")])

    -- Sequences
    it "parses a two-element sequence" $ do
        parseGrammar "Addition ::= Number \"+\" Number ;"
            `shouldBe`
            Right (Grammar [Rule "Addition" (Seq [NonTerm "Number", Term "+", NonTerm "Number"])])

    it "parses a two-element sequence with different operator" $ do
        parseGrammar "Multiplication ::= Number \"*\" Number ;"
            `shouldBe`
            Right (Grammar [Rule "Multiplication" (Seq [NonTerm "Number", Term "*", NonTerm "Number"])])

    it "parses a three-element sequence" $ do
        parseGrammar "FuncCall ::= name \"(\" arg \")\" ;"
            `shouldBe`
            Right (Grammar [Rule "FuncCall" (Seq [NonTerm "name", Term "(", NonTerm "arg", Term ")"])])

    it "parses a sequence of two terminals" $ do
        parseGrammar "Arrow ::= \"-\" \">\" ;"
            `shouldBe`
            Right (Grammar [Rule "Arrow" (Seq [Term "-", Term ">"])])

    -- Alternation
    it "parses alternation of two non-terminals" $ do
        parseGrammar "Expr ::= Number | Identifier ;"
            `shouldBe`
            Right (Grammar [Rule "Expr" (Alt [NonTerm "Number", NonTerm "Identifier"])])

    it "parses alternation of three alternatives" $ do
        parseGrammar "Lit ::= intLit | floatLit | strLit ;"
            `shouldBe`
            Right (Grammar [Rule "Lit" (Alt [NonTerm "intLit", NonTerm "floatLit", NonTerm "strLit"])])

    it "parses alternation of terminals" $ do
        parseGrammar "Bool ::= \"true\" | \"false\" ;"
            `shouldBe`
            Right (Grammar [Rule "Bool" (Alt [Term "true", Term "false"])])

    -- Repetition
    it "parses Many1 (plus)" $ do
        parseGrammar "Digits ::= digit+ ;"
            `shouldBe`
            Right (Grammar [Rule "Digits" (Many1 (NonTerm "digit"))])

    it "parses Many (star)" $ do
        parseGrammar "Spaces ::= space* ;"
            `shouldBe`
            Right (Grammar [Rule "Spaces" (Many (NonTerm "space"))])

    it "parses Many1 on a terminal" $ do
        parseGrammar "Bangs ::= \"!\"+ ;"
            `shouldBe`
            Right (Grammar [Rule "Bangs" (Many1 (Term "!"))])

    -- Optional
    it "parses Optional" $ do
        parseGrammar "Sign ::= \"-\"? ;"
            `shouldBe`
            Right (Grammar [Rule "Sign" (Optional (Term "-"))])

    it "parses Optional non-terminal" $ do
        parseGrammar "MaybeSign ::= sign? ;"
            `shouldBe`
            Right (Grammar [Rule "MaybeSign" (Optional (NonTerm "sign"))])

    -- Grouping / parentheses
    it "parses grouped alternation with repetition" $ do
        parseGrammar "Bits ::= (\"0\" | \"1\")+ ;"
            `shouldBe`
            Right (Grammar [Rule "Bits" (Many1 (Alt [Term "0", Term "1"]))])

    it "parses grouped sequence with optional" $ do
        parseGrammar "Signed ::= (\"-\" digit)? ;"
            `shouldBe`
            Right (Grammar [Rule "Signed" (Optional (Seq [Term "-", NonTerm "digit"]))])

    -- Multiple rules
    it "parses an empty grammar" $ do
        parseGrammar ""
            `shouldBe`
            Right (Grammar [])

    it "parses multiple rules" $ do
        parseGrammar "A ::= \"x\" ; B ::= \"y\" ;"
            `shouldBe`
            Right (Grammar [ Rule "A" (Term "x")
                           , Rule "B" (Term "y") ])

    it "ignores line comments" $ do
        parseGrammar "// a comment\nA ::= \"x\" ;"
            `shouldBe`
            Right (Grammar [Rule "A" (Term "x")])

    it "ignores block comments" $ do
        parseGrammar "/* block */ A ::= \"x\" ;"
            `shouldBe`
            Right (Grammar [Rule "A" (Term "x")])

    -- Error cases
    it "rejects a rule with an empty body" $ do
        parseGrammar "Invalid ::= ;"
            `shouldSatisfy` isLeft

    it "rejects an unclosed string literal" $ do
        parseGrammar "Bad ::= \"unclosed ;"
            `shouldSatisfy` isLeft

    it "rejects a rule missing the separator" $ do
        parseGrammar "NoSep \"x\" ;"
            `shouldSatisfy` isLeft

    it "rejects a rule missing the semicolon" $ do
        parseGrammar "NoSemi ::= \"x\""
            `shouldSatisfy` isLeft

    it "rejects unexpected trailing input" $ do
        parseGrammar "A ::= \"x\" ; !!!"
            `shouldSatisfy` isLeft