{-# LANGUAGE BangPatterns #-}

-- Task 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | p <- primes, q <- primes, p + q == n, p <= q]
  where
    primes = filter isPrime [2 .. n - 2]

-- isPrime :: Int -> Bool
-- isPrime k
--     | k < 2 = False
--     | otherwise = all (\i -> k `mod` i /= 0) [2 .. floor (sqrt (fromIntegral k))]

-- Task 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs (x:xs) = [(x, y) | y <- xs, gcd x y == 1, x < y] ++ coprimePairs xs
coprimePairs [] = []

-- Task 3
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2 .. n]

isPrime :: Int -> Bool
isPrime k
    | k < 2     = False
    | otherwise = k `elem` primesTo k

-- Task 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
  [ [ sum [ a !! i !! k * b !! k !! j | k <- [0 .. p - 1] ]
      | j <- [0 .. n - 1] ]
    | i <- [0 .. m - 1] ]
  where
    m = length a
    p = length (head a)
    n = length (head b)

-- Task 5
permutations :: Eq a => Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations n xs = [x:perm | x <- xs, perm <- permutations (n - 1) (removeFirst x xs)]
  where
    removeFirst _ [] = []
    removeFirst y (z:zs)
        | y == z    = zs
        | otherwise = z : removeFirst y zs

-- Task 6
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x > y = y : merge (x:xs) ys
    | otherwise = y : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming)
                    (merge (map (3*) hamming)
                           (map (5*) hamming))

-- Task 7

power :: Int -> Int -> Int
power x n = go 1 n
  where
    go :: Int -> Int -> Int
    go !acc 0 = acc
    go !acc m = go (acc * x) (m - 1)

-- Task 8
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "Empty list"
listMaxSeq (x:xs) = go x xs
  where
    go currentMax [] = currentMax
    go currentMax (y:ys) =
      let newMax = if y > currentMax then y else currentMax
      in newMax `seq` go newMax ys

listMaxBang :: [Int] -> Int
listMaxBang [] = error "Empty list"
listMaxBang (x:xs) = go x xs
  where
    go !currentMax [] = currentMax
    go !currentMax (y:ys) =
      let newMax = if y > currentMax then y else currentMax
      in go newMax ys

-- Task 9
primes :: [Int]
primes = sieve [2..]

isPrimeInf :: Int -> Bool
isPrimeInf k
    | k < 2     = False
    | otherwise = k `elem` takeWhile (<= k) primes

-- Task 10
mean :: [Double] -> Double
mean [] = error "Empty list"
mean xs = go 0 0 xs
  where
    go accSum accCount [] = accSum / fromIntegral accCount
    go accSum accCount (y:ys) = go (accSum + y) (accCount + 1) ys

meanStrict :: [Double] -> Double
meanStrict [] = error "Empty list"
meanStrict xs = go 0 0 xs
  where
    go :: Double -> Int -> [Double] -> Double
    go !s !n [] = s / fromIntegral n
    go !s !n (x:xs) =
        let !s' = s + x
            !n' = n + 1
        in go s' n' xs

meanVariance :: [Double] -> (Double, Double)
meanVariance [] = error "Empty list"
meanVariance xs = go 0 0 0 xs
  where
    go :: Double -> Double -> Int -> [Double] -> (Double, Double)
    go !s !sq !n [] =
        let mu  = s / fromIntegral n
            var = sq / fromIntegral n - mu * mu
        in (mu, var)
    go !s !sq !n (x:xs) =
        let !s'  = s + x
            !sq' = sq + x * x
            !n'  = n + 1
        in go s' sq' n' xs



main :: IO ()
main = do
    putStrLn "\nTask 1: Goldbach Pairs"
    print (goldbachPairs 10)
    print (goldbachPairs 20) 

    putStrLn "\nTask 2: Coprime Pairs"
    print (coprimePairs [4,7,9,10])
    print (coprimePairs [2,3,4,5,6])

    putStrLn "\nTask 3: Sieve / Primes"
    print (primesTo 20)
    print (isPrime 17)
    print (isPrime 18)

    putStrLn "\nTask 4: Matrix Multiplication"
    let a = [[1,2],[3,4]]
    let b = [[5,6],[7,8]]
    print (matMul a b)

    putStrLn "\nTask 5: Permutations"
    print (permutations 2 [1,2,3])
    print (permutations 3 [1,2,3])

    putStrLn "\nTask 6: Hamming Numbers"
    print (take 10 hamming)
    print (take 20 hamming)

    putStrLn "\nTask 7: Power"
    print (power 2 5)   -- 32
    print (power 3 4)   -- 81

    putStrLn "\nTask 8: List Maximum"
    print (listMaxSeq [1,5,3,9,2])
    print (listMaxBang [1,5,3,9,2])

    putStrLn "\nTask 9: Infinite Primes"
    print (take 10 primes)
    print (isPrimeInf 29)
    print (isPrimeInf 30)

    putStrLn "\nTask 10: Mean"
    print (mean [1,2,3,4])
    print (meanStrict [1,2,3,4])

    putStrLn "\nTask 10: Mean & Variance"
    print (meanVariance [1,2,3,4])
    print (meanVariance [10,20,30])
