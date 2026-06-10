{-# LANGUAGE OverloadedStrings #-}

module EndToEndSpec where

import Test.Hspec
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

import GrammarParser (parseGrammar)
import Interpreter (parseInput, ParseTree(..))
import LeftRecursion (detectLeftRecursion)

run :: String -> T.Text -> Either String ParseTree
run grammarSrc input =
    case parseGrammar (T.pack grammarSrc) of
        Left err -> Left ("Grammar parse error: " ++ show err)
        Right grammar -> parseInput grammar input

spec :: Spec
spec = describe "End-to-end" $ do

    describe "identifier grammar" $ do
        let g = "Var ::= letter (letter | digit)* ;"

        it "accepts a single letter" $
            run g "x" `shouldSatisfy` isRight

        it "accepts a letter followed by digits" $
            run g "x123" `shouldSatisfy` isRight

        it "accepts a mixed alphanumeric identifier" $
            run g "abc12" `shouldSatisfy` isRight

        it "rejects a string starting with a digit" $
            run g "1abc" `shouldSatisfy` isLeft

        it "rejects an empty string" $
            run g "" `shouldSatisfy` isLeft


    describe "natural number grammar" $ do
        let g = "Number ::= digit+ ;"

        it "accepts a single digit" $
            run g "0" `shouldSatisfy` isRight

        it "accepts multiple digits" $
            run g "42" `shouldSatisfy` isRight

        it "accepts a long number" $
            run g "1234567890" `shouldSatisfy` isRight

        it "rejects an empty string" $
            run g "" `shouldSatisfy` isLeft

        it "rejects a non-digit character" $
            run g "a" `shouldSatisfy` isLeft

        it "rejects digits with a trailing letter" $
            run g "12x" `shouldSatisfy` isLeft
        
        it "rejects a number with a decimal point" $
            run g "3.14" `shouldSatisfy` isLeft


    describe "boolean literal grammar" $ do
        let g = "Bool ::= \"true\" | \"false\" ;"

        it "accepts \"true\"" $
            run g "true" `shouldSatisfy` isRight

        it "accepts \"false\"" $
            run g "false" `shouldSatisfy` isRight

        it "rejects \"True\" (case-sensitive)" $
            run g "True" `shouldSatisfy` isLeft

        it "rejects an empty string" $
            run g "" `shouldSatisfy` isLeft

        it "rejects a partial match" $
            run g "tru" `shouldSatisfy` isLeft


    describe "simple assignment grammar" $ do
        let g = "Assignment ::= letter \"=\" digit ;"

        it "accepts a well-formed assignment" $
            run g "x=5" `shouldSatisfy` isRight

        it "rejects missing equals sign" $
            run g "x5" `shouldSatisfy` isLeft

        it "rejects reversed operands" $
            run g "5=x" `shouldSatisfy` isLeft

        it "rejects trailing input" $
            run g "x=5y" `shouldSatisfy` isLeft

        it "rejects 2 numbers" $
            run g "5=6" `shouldSatisfy` isLeft


    describe "optional sign grammar" $ do
        let g = "Signed ::= \"-\"? digit+ ;"

        it "accepts a plain number" $
            run g "42" `shouldSatisfy` isRight

        it "accepts a negative number" $
            run g "-7" `shouldSatisfy` isRight

        it "rejects a lone minus sign" $
            run g "-" `shouldSatisfy` isLeft

  
    describe "binary string grammar" $ do
        let g = "Bit ::= (\"0\" | \"1\")+ ;"

        it "accepts a single 0" $
            run g "0" `shouldSatisfy` isRight

        it "accepts a single 1" $
            run g "1" `shouldSatisfy` isRight

        it "accepts a longer binary string" $
            run g "10110" `shouldSatisfy` isRight

        it "rejects an empty string" $
            run g "" `shouldSatisfy` isLeft

        it "rejects a non-binary character" $
            run g "2" `shouldSatisfy` isLeft


    describe "multi-rule grammar" $ do
        let g = "Assign ::= letter \"=\" Expr ; Expr ::= digit+ ;"

        it "accepts a valid assignment expression" $
            run g "x=99" `shouldSatisfy` isRight

        it "rejects a missing value" $
            run g "x=" `shouldSatisfy` isLeft

        it "rejects a non-terminal value" $
            run g "x=y" `shouldSatisfy` isLeft


    describe "right-recursive addition grammar" $ do
        let g = unlines
                [ "Expr   ::= Number (\"+\" Expr | \"-\" Expr)? ;"
                , "Number ::= digit+ ;"
                ]

        it "accepts a single number" $
            run g "42" `shouldSatisfy` isRight

        it "accepts a simple addition" $
            run g "1+2" `shouldSatisfy` isRight

        it "accepts a simple subtraction" $
            run g "10-3" `shouldSatisfy` isRight

        it "accepts a chain of additions (right-associative)" $
            run g "1+2+3" `shouldSatisfy` isRight

        it "accepts a longer chain" $
            run g "1+22+333+4" `shouldSatisfy` isRight

        it "rejects a leading operator" $
            run g "+1" `shouldSatisfy` isLeft

        it "rejects a trailing operator" $
            run g "1+" `shouldSatisfy` isLeft

        it "rejects non-digit input" $
            run g "a+b" `shouldSatisfy` isLeft

        it "rejects an empty string" $
            run g "" `shouldSatisfy` isLeft


    describe "right-recursive multiplication grammar" $ do
        let g = unlines
                [ "Expr   ::= Number (\"*\" Expr)? ;"
                , "Number ::= digit+ ;"
                ]

        it "accepts a single number" $
            run g "7" `shouldSatisfy` isRight

        it "accepts a simple multiplication" $
            run g "3*4" `shouldSatisfy` isRight

        it "accepts a chain of multiplications" $
            run g "2*3*4" `shouldSatisfy` isRight

        it "rejects a missing right operand" $
            run g "3*" `shouldSatisfy` isLeft

 
    describe "two-level expression grammar (Expr / Term)" $ do
        let g = unlines
                [ "Expr ::= Term (\"+\" Expr)? ;"
                , "Term ::= digit (\"*\" Term)? ;"
                ]

        it "accepts a single digit" $
            run g "5" `shouldSatisfy` isRight

        it "accepts an addition of two digits" $
            run g "2+3" `shouldSatisfy` isRight

        it "accepts a multiplication of two digits" $
            run g "2*3" `shouldSatisfy` isRight

        it "accepts mixed addition and multiplication" $
            run g "2*3+4" `shouldSatisfy` isRight

        it "accepts a longer mixed expression" $
            run g "1+2*3+4" `shouldSatisfy` isRight

        it "rejects a leading operator" $
            run g "*3" `shouldSatisfy` isLeft

        it "rejects letters" $
            run g "a+b" `shouldSatisfy` isLeft

        it "rejects an empty string" $
            run g "" `shouldSatisfy` isLeft


    describe "identifier grammar with helper rule" $ do
        let g = unlines
                [ "Var   ::= letter AlNum* ;"
                , "AlNum ::= letter | digit ;"
                ]

        it "accepts a single letter" $
            run g "x" `shouldSatisfy` isRight

        it "accepts a multi-character identifier" $
            run g "abc" `shouldSatisfy` isRight

        it "accepts an identifier with digits" $
            run g "x1y2" `shouldSatisfy` isRight

        it "rejects a digit-only string" $
            run g "123" `shouldSatisfy` isLeft

        it "rejects an empty string" $
            run g "" `shouldSatisfy` isLeft

        it "rejects a string starting with a digit" $
            run g "1abc" `shouldSatisfy` isLeft


    describe "recursive list grammar" $ do
        let g = unlines
                [ "List  ::= \"[\" Items \"]\" ;"
                , "Items ::= digit (\",\" Items)? ;"
                ]

        it "accepts a single-element list" $
            run g "[1]" `shouldSatisfy` isRight

        it "accepts a multi-element list" $
            run g "[1,2,3]" `shouldSatisfy` isRight

        it "rejects an empty list" $
            run g "[]" `shouldSatisfy` isLeft

        it "rejects a missing closing bracket" $
            run g "[1,2" `shouldSatisfy` isLeft

        it "rejects a trailing comma" $
            run g "[1,2,]" `shouldSatisfy` isLeft

        it "rejects non-digit items" $
            run g "[a,b]" `shouldSatisfy` isLeft


    describe "left-recursion detection" $ do
        it "refuses to parse when grammar is directly left-recursive" $
            run "A ::= A \"x\" ;" "x" `shouldSatisfy` isLeft

        it "refuses to load a grammar if any rule is left-recursive" $
            run "A ::= \"a\" ; B ::= B \"b\" ;" "a" `shouldSatisfy` isLeft