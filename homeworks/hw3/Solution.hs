import Control.Monad (foldM, guard)
import Data.List (permutations)
import Data.Map
import qualified Data.Set as Set
import Control.Monad.Writer

-- Task 1
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)
-- Subtask a
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    neighbors <- Data.Map.lookup pos maze
    Data.Map.lookup dir neighbors
-- Subtask b
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze = foldM (\pos dir -> move maze pos dir)
-- Subtask c
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = do
    (_, path) <- foldM step (start, [start]) dirs
    pure path
  where
    step (pos, path) dir = do
        next <- move maze pos dir
        pure (next, path ++ [next])

-- Task 2
type Key = Map Char Char
decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Data.Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- Task 3
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    seating <- permutations guests
    guard (isValid seating)
    pure seating
  where
    conflictSet = Set.fromList $
        concatMap (\(g1, g2) -> [(g1, g2), (g2, g1)]) conflicts

    isValid seating = all (`Set.notMember` conflictSet) (pairs seating)
      where
        pairs [] = []
        pairs xs = zip xs (tail xs ++ [head xs])

-- Task 4
data Result a = Failure String | Success a [String]
-- Subtask a
instance Functor Result where
    fmap f (Failure msg) = Failure msg
    fmap f (Success x logs) = Success (f x) logs
instance Applicative Result where
    pure x = Success x []
    Failure msg <*> _ = Failure msg
    Success f logs1 <*> r = case r of
        Failure msg -> Failure msg
        Success x logs2 -> Success (f x) (logs1 ++ logs2)
instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success x logs >>= f = case f x of
        Failure msg -> Failure msg
        Success y logs2 -> Success y (logs ++ logs2)
-- Subtask b
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- Subtask c
validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "Age cannot be negative"
    | age > 150 = Success age ["Age looks unusually high, please double-check"]
    | otherwise = Success age []

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- Task 5
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
    deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = pure (Lit n)

simplify (Add e1 e2) = do
    s1 <- simplify e1
    s2 <- simplify e2
    case (s1, s2) of
        (Lit 0, e) -> do
            tell ["Add identity: 0 + e -> e"]
            pure e
        (e, Lit 0) -> do
            tell ["Add identity: e + 0 -> e"]
            pure e
        (Lit a, Lit b) -> do
            tell ["Add constant folding"]
            pure (Lit (a + b))
        _ ->
            pure (Add s1 s2)

simplify (Mul e1 e2) = do
    s1 <- simplify e1
    s2 <- simplify e2
    case (s1, s2) of
        (Lit 1, e) -> do
            tell ["Mul identity: 1 * e -> e"]
            pure e
        (e, Lit 1) -> do
            tell ["Mul identity: e * 1 -> e"]
            pure e
        (Lit 0, _) -> do
            tell ["Mul zero: 0 * e -> 0"]
            pure (Lit 0)
        (_, Lit 0) -> do
            tell ["Mul zero: e * 0 -> 0"]
            pure (Lit 0)
        (Lit a, Lit b) -> do
            tell ["Mul constant folding"]
            pure (Lit (a * b))
        _ ->
            pure (Mul s1 s2)

simplify (Neg e) = do
    s <- simplify e
    case s of
        Neg inner -> do
            tell ["Double negation: -(-e) -> e"]
            pure inner
        _ ->
            pure (Neg s)

-- Task 6
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)
-- Subtask a
instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (Prelude.map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- Subtask b
test1 :: ZipList Int
test1 = pure id <*> ZipList [1,2,3]

test2 :: ZipList Int
test2 = pure (+) <*> ZipList [1,2,3] <*> ZipList [10,20,30]

-- Subtask c
-- ZipList cannot have a lawful Monad instance because (>>=) would need to
-- combine the ZipLists produced for each element into one final ZipList.
-- If the function returns lists of different lengths, there is no consistent
-- way to preserve positional zipping. Any flattening strategy would break
-- the zip-based Applicative behavior, so the monad laws cannot hold.