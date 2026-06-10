-- Grammar: Addition ::= Addition "+" digit | digit ;
module StringGenerators.Addition where

import System.Random (randomRIO)

generateAddition :: IO String
generateAddition = do
    len <- randomRIO (1, 5 :: Int)
    digits <- sequence [randomNumber | _ <- [1..len]]
    let expression = foldr1 (\d acc -> d ++ "+" ++ acc) digits
    pure expression

randomNumber :: IO String
randomNumber = do
    n <- randomRIO (0, 9 :: Int)
    sequence [randomDigit | _ <- [1..n+1]]

randomDigit :: IO Char
randomDigit = do
    n <- randomRIO (0, 9 :: Int)
    pure (toEnum (fromEnum '0' + n))