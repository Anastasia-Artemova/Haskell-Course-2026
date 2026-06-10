module StringGenerators.Assignment where

import System.Random (randomRIO)

generateAssignment :: IO String
generateAssignment = do
  name <- randomString
  value <- randomNumber
  pure $ name ++ "=" ++ value

randomString :: IO String
randomString = do
    len <- randomRIO (1, 10 :: Int)
    sequence [randomLetter | _ <- [1..len]]

randomNumber :: IO String
randomNumber = do
    len <- randomRIO (1, 5 :: Int)
    sequence [randomDigit | _ <- [1..len]]

randomLetter :: IO Char
randomLetter = do
    n <- randomRIO (0, 25 :: Int)
    pure (toEnum (fromEnum 'a' + n))

randomDigit :: IO Char
randomDigit = do
    n <- randomRIO (0, 9 :: Int)
    pure (toEnum (fromEnum '0' + n))