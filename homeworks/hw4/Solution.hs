
{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }

-- Task 1
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader (\_ -> x)

  liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f (Reader g) (Reader h) = Reader (\r -> f (g r) (h r))

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader g) >>= f = Reader (\r -> runReader (f (g r)) r)

-- Task 2
ask   :: Reader r r
ask = Reader id

asks  :: (r -> a) -> Reader r a
asks = Reader

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader (g . f)

-- Task 3
data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)


calculateInterest   :: Account -> Reader BankConfig Int
calculateInterest account = do 
    BankConfig { interestRate } <- ask
    let interest = floor (fromIntegral (balance account) * interestRate)
    return interest

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    BankConfig { transactionFee } <- ask
    let updatedBalance = balance account - transactionFee
    return account { balance = updatedBalance }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    BankConfig { minimumBalance } <- ask
    return (balance account >= minimumBalance)

processAccount      :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    interest <- calculateInterest account
    updatedAccount <- applyTransactionFee account
    meetsMinimum <- checkMinimumBalance account
    return (updatedAccount, interest, meetsMinimum)

main :: IO ()
main = do
    let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
    let acc = Account { accountId = "A-001", balance = 1000 }
    print $ runReader (processAccount acc) cfg
