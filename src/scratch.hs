import Data.List (foldl')

data Transaction = Transaction
  { sender :: String
  , receiver :: String
  , value :: Int
  , timeProcessed :: Int
  } deriving (Show, Read, Eq)

data Block = Block { index        :: Int
                   , previousHash :: String
                   , timestamp    :: Int
                   , blockData    :: [Transaction]
                   , nonce        :: Int
                   , blockHash    :: String
                   } deriving (Show, Read, Eq)

testChain = [Block 1 "a3f" 1001 [Transaction "Corey" "Lyndon" 12 10, Transaction "Corey" "Lyndon" 12 10] 40 "a3f", Block 2 "a3f" 1001 [Transaction "Lyndon" "Corey" 100 10, Transaction "Corey" "Shauna" 12 10] 12 "ab2"]


-- sumDebits :: (Ord s, Ord r, Num a, Num t) => [(s, r, a, t)] -> Map r a
-- sumDebits list = foldl' (\m (s, r, a, t) -> alter (Just . maybe a (+ a)) r m) empty list
--
-- sumCredits :: (Ord s, Ord r, Num a, Num t) => [(s, r, a, t)] -> Map s a
-- sumCredits list = foldl' (\m (s, r, a, t) -> alter (Just . maybe a (+ a)) s m) empty list

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

transactions :: [Transaction]
transactions = flatten [ transactions | Block _ _ _ transactions _ _  <- testChain]

filterBySender :: String -> Transaction -> Bool
filterBySender key (Transaction sender _ _ _ ) | sender == key = True
filterBySender _ _ = False

filterByReceiver :: String -> Transaction -> Bool
filterByReceiver key (Transaction _ receiver _ _ ) | receiver == key = True
filterByReceiver _ _ = False

credit :: String -> Int
credit key = foldl (\acc (Transaction _ _ amount _)  -> acc + amount ) 0 $ filter (filterBySender key) transactions

debit :: String -> Int
debit key = foldl (\acc (Transaction _ _ amount _)  -> acc + amount ) 0 $ filter (filterByReceiver key) transactions

balance :: String -> Int
balance key = debit key - credit key
