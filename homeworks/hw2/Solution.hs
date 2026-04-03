import Data.Foldable
import Data.Sequence (Seq)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)

-- Task 1
instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append s1 s2) = Append (fmap f s1) (fmap f s2)

-- Task 2
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append s1 s2) = foldMap f s1 <> foldMap f s2

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- Task 3
instance Semigroup (Sequence a) where
    Empty <> s = s
    s <> Empty = s
    s1 <> s2 = Append s1 s2

instance Monoid (Sequence a) where
    mempty = Empty

-- Task 4
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target seq = go [seq]
  where
    go [] = False
    go (s:stack) = case s of 
        Empty -> go stack
        Single x -> if x == target then True else go stack
        Append s1 s2 -> go (s1 : s2 : stack)

-- Task 5
tailToList :: Sequence a -> [a]
tailToList seq = reverse (go [seq] [])
    where
        go [] acc = acc
        go (s:stack) acc = case s of
            Empty -> go stack acc
            Single x -> go stack (x : acc)
            Append s1 s2 -> go (s1 : s2 : stack) acc

-- Task 6
data Token = TNum Int | TAdd | TSub | TMul | TDiv
    deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
    where 
        go [] [result] = Just result
        go [] _ = Nothing
        go (t:ts) stack = case t of
            TNum n -> go ts (n : stack)
            TAdd -> applyOp (+) ts stack
            TSub -> applyOp (-) ts stack
            TMul -> applyOp (*) ts stack
            TDiv -> applyDiv ts stack

        applyOp op ts (y:x:stack) = go ts (x `op` y : stack)
        applyOp _ _ _ = Nothing

        applyDiv ts (y:x:stack) = if y == 0 then Nothing else go ts (x `div` y : stack)
        applyDiv _ _ = Nothing

-- Task 7
-- Subtask a
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Subtask b
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- Subtask c
decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0

-- Task 8
-- Subtask a
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr go []
    where 
        go x [] = [(x, 1)]
        go x ((y, count):rest)
            | x == y    = (y, count + 1) : rest
            | otherwise = (x, 1) : (y, count) : rest

-- Subtask b
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, count) acc -> replicate count x ++ acc) []

main :: IO ()
main = do
    let seq1 :: Sequence Int
        seq1 = Append (Single 1) (Append (Single 2) (Single 3))

    let seq2 :: Sequence Int
        seq2 = Append Empty (Append (Single 4) Empty)

    let seq3 :: Sequence Int
        seq3 = (Single 1) <> (Single 2) <> (Single 3)

    putStrLn "=== Task 1 ==="
    print (fmap (+1) seq1 == Append (Single 2) (Append (Single 3) (Single 4)))

    putStrLn "=== Task 2 ==="
    print (seqToList seq1 == [1,2,3])
    print (seqLength seq1 == 3)
    print (seqToList seq2 == [4])
    print (seqLength (Empty :: Sequence Int) == 0)

    putStrLn "=== Task 3 ==="
    print ((mempty <> seq1) == seq1)
    print ((seq1 <> mempty) == seq1)
    print (seqToList seq3 == [1,2,3])

    putStrLn "=== Task 4 ==="
    print (tailElem 2 seq1 == True)
    print (tailElem 5 seq1 == False)
    print (tailElem 4 seq2 == True)

    putStrLn "=== Task 5 ==="
    print (tailToList seq1 == [1,2,3])
    print (tailToList seq2 == [4])
    print (tailToList (Empty :: Sequence Int) == [])

    putStrLn "=== Task 6 ==="
    print (tailRPN [TNum 2, TNum 3, TAdd] == Just 5)
    print (tailRPN [TNum 10, TNum 2, TSub, TNum 3, TMul] == Just 24)
    print (tailRPN [TAdd] == Nothing)
    print (tailRPN [TNum 10, TNum 0, TDiv] == Nothing)

    putStrLn "=== Task 7 ==="
    print (myReverse [1,2,3] == [3,2,1])
    print (myTakeWhile (<3) [1,2,3,1] == [1,2])
    print (decimal [1,2,3,4] == 1234)

    putStrLn "=== Task 8 ==="
    print (encode "aaabccca" == [('a',3),('b',1),('c',3),('a',1)])
    print (decode [('a',3),('b',1),('c',3),('a',1)] == "aaabccca")
    print (decode (encode "aaabccca") == "aaabccca")