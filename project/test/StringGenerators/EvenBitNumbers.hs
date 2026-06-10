-- Grammar:EvenBits ::= ("0" | "1")* "0" ;
module StringGenerators.EvenBitNumbers where

import System.Random (randomRIO)

generateEvenBitNumber :: IO String
generateEvenBitNumber = do
  len <- randomRIO (0, 10 :: Int)
  bits <- sequence [randomBit | _ <- [1..len]]
  pure (bits ++ "0")

randomBit :: IO Char
randomBit = do
  n <- randomRIO (0, 1 :: Int)
  pure (if n == 0 then '0' else '1')