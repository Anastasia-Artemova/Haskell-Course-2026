import Control.Monad.State
import Control.Monad (unless)
import qualified Data.Map as Map
import Data.Map (Map)

-- Task 1
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> Control.Monad.State.State [Int] ()
execInstr inst = case inst of
    PUSH n -> modify (n :)

    POP -> do
        stack <- get
        case stack of
            (_:xs) -> put xs
            [] -> return ()

    DUP -> do
        stack <- get
        case stack of
            (x:xs) -> put (x:x:xs)
            [] -> return ()

    SWAP -> do
        stack <- get
        case stack of
            (x:y:xs) -> put (y:x:xs)
            _ -> return ()

    ADD -> do
        stack <- get
        case stack of
            (x:y:xs) -> put ((x + y):xs)
            _ -> return ()

    MUL -> do
        stack <- get
        case stack of
            (x:y:xs) -> put ((x * y):xs)
            _ -> return ()

    NEG -> do
        stack <- get
        case stack of
            (x:xs) -> put ((-x):xs)
            [] -> return ()

execProg :: [Instr] -> Control.Monad.State.State [Int] ()
execProg [] = return ()
execProg (inst:insts) = do
    execInstr inst
    execProg insts

runProg :: [Instr] -> [Int]
runProg [] = []
runProg prog = execState (execProg prog) []

-- Task 2
data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq  Expr Expr


eval :: Expr -> Control.Monad.State.State (Map String Int) Int
eval exp = case exp of
    Num n -> return n

    Var name -> do
        env <- get
        return (env Map.! name)

    Add e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 + v2)

    Mul e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 * v2)

    Neg e -> do
        v <- eval e
        return (-v)

    Assign name e -> do
        v <- eval e
        modify (Map.insert name v)
        return v

    Seq e1 e2 -> do
        eval e1
        eval e2

runEval :: Expr -> Int
runEval exp = evalState (eval exp) Map.empty

-- Task 3
editDistM :: String -> String -> Int -> Int -> Control.Monad.State.State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just value -> return value
        Nothing -> do
            value <-
                if i == 0 then
                    return j
                else if j == 0 then
                    return i
                else if xs !! (i - 1) == ys !! (j - 1) then
                    editDistM xs ys (i - 1) (j - 1)
                else do
                    deletion <- editDistM xs ys (i - 1) j
                    insertion <- editDistM xs ys i (j - 1)
                    substitution <- editDistM xs ys (i - 1) (j - 1)
                    return (1 + minimum [deletion, insertion, substitution])

            modify (Map.insert (i, j) value)
            return value

editDistance :: String -> String -> Int
editDistance str1 str2 = evalState (editDistM str1 str2 (length str1) (length str2)) Map.empty

-- Task 4
data Location = Normal | DecisionPoint | Obstacle | Treasure | Trap| Goal deriving Show

data GameState = GameState { position :: Int, energy  :: Int, score   :: Int, location :: Location} deriving Show
type AdventureGame a = StateT GameState IO a

getLocation :: Int -> Location
getLocation pos
    | pos >= 20 = Goal
    | pos `elem` [5, 12] = Obstacle
    | pos `elem` [3, 10, 17] = Treasure
    | pos `elem` [8, 15] = Trap
    | pos `elem` [6, 14] = DecisionPoint
    | otherwise = Normal

movePlayer :: Int -> AdventureGame Int
movePlayer moves = do
    playerState <- get

    let newPos = position playerState + moves
    let newEnergy = energy playerState - moves
    let newLocation = getLocation newPos

    put playerState
        { position = newPos
        , energy = newEnergy
        , location = newLocation
        }

    return moves

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    lift $ putStrLn "Choose a path:"
    lift $
        mapM_
            (\(i, opt) -> putStrLn (show i ++ ". " ++ opt))
            (zip [1..] options)

    choice <- lift getLine
    let index = read choice - 1
    return (options !! index)

-- Task 5
handleLocation :: AdventureGame Bool
handleLocation = do
    playerState <- get
    case location playerState of
        Normal -> return False
        DecisionPoint -> do
            choice <- makeDecision ["Left path", "Right path"]
            lift $ putStrLn ("You chose: " ++ choice)
            return False
        Obstacle -> do
            let newScore = max 0 (score playerState - 1)
            put playerState { score = newScore }
            return False
        Treasure -> do
            let newScore = score playerState + 1
            put playerState { score = newScore }
            return False
        Trap -> do
            let newScore = max 0 (score playerState - 3)
            put playerState { score = newScore }
            return False
        Goal -> do
            lift $ putStrLn "You reached the treasure!"
            return True

playTurn :: AdventureGame Bool
playTurn = do
    roll <- lift getDiceRoll
    movePlayer roll

    playerState <- get
    lift (displayGameState playerState)

    if energy playerState <= 0
    then do
        lift $ putStrLn "You ran out of energy!"
        return True
    else handleLocation

playGame :: AdventureGame ()
playGame = do
    ended <- playTurn
    unless ended playGame

-- Task 6
getDiceRoll :: IO Int
getDiceRoll = do
    putStrLn "Provide a dice roll:"
    read <$> getLine

displayGameState :: GameState -> IO ()
displayGameState state = do
    putStrLn "Current game state:"
    print state

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose one option:"

    mapM_
        (\(i, option) -> putStrLn (show i ++ ". " ++ option))
        (zip [1..] options)

    input <- getLine
    let index = read input - 1

    return (options !! index)


testStack1 :: [Int]
testStack1 = runProg [PUSH 3, PUSH 4, ADD]

testStack2 :: [Int]
testStack2 = runProg [PUSH 2, PUSH 5, MUL, NEG]

testStack3 :: [Int]
testStack3 = runProg [POP, PUSH 1, DUP, ADD]

testExpr1 :: Int
testExpr1 =
    runEval (Add (Num 2) (Num 3))

testExpr2 :: Int
testExpr2 =
    runEval
        (Seq
            (Assign "x" (Num 10))
            (Add (Var "x") (Num 5)))

testExpr3 :: Int
testExpr3 =
    runEval
        (Seq
            (Assign "x" (Num 4))
            (Seq
                (Assign "y" (Mul (Var "x") (Num 3)))
                (Add (Var "x") (Var "y"))))

testEdit1 :: Int
testEdit1 = editDistance "cat" "cut"

testEdit2 :: Int
testEdit2 = editDistance "kitten" "sitting"

testEdit3 :: Int
testEdit3 = editDistance "" "abc"

initialGameState :: GameState
initialGameState =
    GameState
        { position = 0
        , energy = 10
        , score = 0
        , location = Normal
        }

testMovePlayer :: IO ()
testMovePlayer = do
    finalState <- execStateT (movePlayer 3) initialGameState
    print finalState

main :: IO ()
main = do
    print testStack1
    print testStack2
    print testStack3

    print testExpr1
    print testExpr2
    print testExpr3

    print testEdit1
    print testEdit2
    print testEdit3

    testMovePlayer



