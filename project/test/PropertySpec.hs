{-# LANGUAGE OverloadedStrings #-}

module PropertySpec where

import Test.Hspec
import Data.Either (isRight, isLeft)
import qualified Data.Text as T
import System.Random (randomRIO)

import GrammarParser
import Interpreter
import StringGenerators.EvenBitNumbers
import StringGenerators.Assignment
import StringGenerators.Addition


evenBitsGrammar :: Grammar
evenBitsGrammar = Grammar
    [ Rule "EvenBits" (Seq [Many (Alt [Term "0", Term "1"]), Term "0"]) ]

additionGrammar :: Grammar
additionGrammar = Grammar
    [ Rule "Expr"   (Seq [NonTerm "Number", Many (Seq [Term "+", NonTerm "Number"])])
    , Rule "Number" (Many1 (NonTerm "digit"))
    ]

assignmentGrammar :: Grammar
assignmentGrammar = Grammar
    [ Rule "Assign" (Seq [NonTerm "Var", Term "=", NonTerm "Expr"])
    , Rule "Var"    (Seq [NonTerm "letter", Many (Alt [NonTerm "letter", NonTerm "digit"])])
    , Rule "Expr"   (Many1 (NonTerm "digit"))
    ]

accepts :: Grammar -> String -> Bool
accepts g s = isRight (parseInput g (T.pack s))

rejectionRate :: Grammar -> [String] -> Double
rejectionRate g ss =
    fromIntegral (length (filter (not . accepts g) ss))
    / fromIntegral (max 1 (length ss))


genericChars :: [Char]
genericChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ".?!@#$%^&*"

randomFrom :: [a] -> IO a
randomFrom xs = (xs !!) <$> randomRIO (0, length xs - 1)

genericDelete :: String -> IO String
genericDelete [] = pure []
genericDelete s  = do
    i <- randomRIO (0, length s - 1)
    let (l, _:r) = splitAt i s
    pure (l ++ r)

genericReplace :: String -> IO String
genericReplace [] = pure "x"
genericReplace s  = do
    c <- randomFrom genericChars
    i <- randomRIO (0, length s - 1)
    let (l, _:r) = splitAt i s
    pure (l ++ [c] ++ r)

genericInsert :: String -> IO String
genericInsert s = do
    c   <- randomFrom genericChars
    pos <- randomRIO (0, length s)
    let (l, r) = splitAt pos s
    pure (l ++ [c] ++ r)

-- EvenBits targeted mutators 
mutateEvenBits :: String -> IO String
mutateEvenBits s = do
    op <- randomRIO (0, 3 :: Int)
    case op of
        0 -> pure $ if null s then "1" else init s ++ "1"
        1 -> pure (s ++ "1")
        2 -> do
                c <- randomFrom ".?!@#abcxyz"
                pos <- randomRIO (0, length s)
                let (l, r) = splitAt pos s
                pure (l ++ [c] ++ r)
        _ -> if null s
                then pure "x"
                else do
                    c <- randomFrom ".?!@#abcxyz"
                    i <- randomRIO (0, length s - 1)
                    let (l, _:r) = splitAt i s
                    pure (l ++ [c] ++ r)

-- Addition targeted mutators 
mutateAddition :: String -> IO String
mutateAddition s = do
    op <- randomRIO (0, 5 :: Int)
    let plusPositions  = [i | (i, c) <- zip [0..] s, c == '+']
        digitPositions = [i | (i, c) <- zip [0..] s, c `elem` ['0'..'9']]
    case op of
        0 -> case plusPositions of
                [] -> genericDelete s
                ps -> do i <- randomFrom ps
                         let (l, _:r) = splitAt i s
                         pure (l ++ r)
        1 -> case plusPositions of
                [] -> genericReplace s
                ps -> do i  <- randomFrom ps
                         op2 <- randomFrom "-*/"
                         let (l, _:r) = splitAt i s
                         pure (l ++ [op2] ++ r)
        2 -> case plusPositions of
                [] -> pure (s ++ "+")
                ps -> do i <- randomFrom ps
                         let (l, r) = splitAt i s
                         pure (l ++ "++" ++ r)
        3 -> case digitPositions of
                [] -> genericReplace s
                ds -> do i <- randomFrom ds
                         c <- randomFrom ['a'..'z']
                         let (l, _:r) = splitAt i s
                         pure (l ++ [c] ++ r)
        4 -> pure (s ++ "+")
        _ -> pure ('+' : s)

-- Assignment targeted mutators 
mutateAssignment :: String -> IO String
mutateAssignment s = do
    op <- randomRIO (0, 6 :: Int)
    case break (== '=') s of
        (var, '=':val) ->
            case op of
                0 -> pure (var ++ val)
                1 -> pure (var ++ "==" ++ val)
                2 -> pure (var ++ ":" ++ val)
                3 -> if null var
                        then pure ("1=" ++ val)
                        else do d <- randomFrom ['0'..'9']
                                pure (d : tail var ++ "=" ++ val)
                4 -> let ds = [i | (i,c) <- zip [0..] val, c `elem` ['0'..'9']]
                     in case ds of
                            [] -> genericReplace s
                            _  -> do i <- randomFrom ds
                                     c <- randomFrom ['a'..'z']
                                     let (l, _:r) = splitAt i val
                                     pure (var ++ "=" ++ l ++ [c] ++ r)
                5 -> do c <- randomFrom ['a'..'z']
                        pure (s ++ [c])
                _ -> pure (var ++ "=")
        _ -> genericInsert s


spec :: Spec
spec = describe "Property-like tests" $ do

    describe "EvenBits grammar" $ do

        it "accepts all generated even-bit strings" $ do
            samples <- sequence [generateEvenBitNumber | _ <- [1..100]]
            mapM_ (\s -> parseInput evenBitsGrammar (T.pack s)
                            `shouldSatisfy` isRight) samples

        it "rejects known-invalid strings" $
            mapM_ (\s -> parseInput evenBitsGrammar (T.pack s)
                            `shouldSatisfy` isLeft)
                  ["1", "01", "101", "111", "x0", "0x", "", "2"]

        it "rejects >90% of targeted mutations" $ do
            originals <- sequence [generateEvenBitNumber | _ <- [1..200]]
            mutated <- mapM mutateEvenBits originals
            rejectionRate evenBitsGrammar mutated `shouldSatisfy` (> 0.90)

    -- Addition
    describe "Addition grammar" $ do

        it "accepts all generated addition expressions" $ do
            samples <- sequence [generateAddition | _ <- [1..100]]
            mapM_ (\s -> parseInput additionGrammar (T.pack s) `shouldSatisfy` isRight) samples

        it "accepts known-valid strings" $
            mapM_ (\s -> parseInput additionGrammar (T.pack s) `shouldSatisfy` isRight)
                  ["1", "1+2", "123+456", "0+0", "1+2+3", "99"]

        it "rejects known-invalid strings" $
            mapM_ (\s -> parseInput additionGrammar (T.pack s) `shouldSatisfy` isLeft)
                  ["1+", "+2", "a+b", "", "1++2", "+", "1-2", "1*2"]

        it "rejects >75% of targeted mutations" $ do
            originals <- sequence [generateAddition | _ <- [1..200]]
            mutated <- mapM mutateAddition originals
            rejectionRate additionGrammar mutated `shouldSatisfy` (> 0.75)

    -- Assignment
    describe "Assignment grammar" $ do

        it "accepts all generated assignment expressions" $ do
            samples <- sequence [generateAssignment | _ <- [1..100]]
            mapM_ (\s -> parseInput assignmentGrammar (T.pack s) `shouldSatisfy` isRight) samples

        it "accepts known-valid strings" $
            mapM_ (\s -> parseInput assignmentGrammar (T.pack s) `shouldSatisfy` isRight)
                  ["x=5", "var123=456", "a=0", "abc=99"]

        it "rejects known-invalid strings" $
            mapM_ (\s -> parseInput assignmentGrammar (T.pack s) `shouldSatisfy` isLeft)
                  ["=5", "x=", "x=5+2", "1var=5", "", "x==5", "5=5"]

        it "rejects >85% of targeted mutations" $ do
            originals <- sequence [generateAssignment | _ <- [1..200]]
            mutated <- mapM mutateAssignment originals
            rejectionRate assignmentGrammar mutated `shouldSatisfy` (> 0.85)