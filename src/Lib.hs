{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, ParallelListComp #-}

-- LEGION CODE SOURCED FROM: https://github.com/aviaviavi/legion/blob/master/src/Lib.hs --
module Lib where


-- LEGION: Package selection --
import           System.IO.Unsafe
import           Control.Monad.Trans
import           Crypto.Hash                    ( Digest
                                                , SHA256
                                                , digestFromByteString
                                                )
import           Crypto.Hash.SHA256
import           Data.Aeson
import           Data.Binary
import           Data.ByteString.Char8          (pack)
import           Data.Time.Clock.POSIX
import           Text.Read                      (readMaybe)
import           GHC.Generics                   hiding (to, from)
import           Web.Spock
import           Web.Spock.Config

-- TODO: Need to refactor out
---------------------------------

epoch :: IO Int
epoch = round `fmap` getPOSIXTime

sha256 :: String -> Maybe (Digest SHA256)
sha256 = digestFromByteString . hash . pack

hashString :: String -> String
hashString = maybe (error "Something went wrong generating a hash") show . sha256

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

-- TODO: Need to refactor out
----------------------------------
data Transaction = Transaction
  { tid :: String
  , sender :: String
  , receiver :: String
  , value :: Int
  , timeProcessed :: Int
  } deriving (Show, Read, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction
instance Binary Transaction

data TransactionArgs = TransactionArgs
  { to :: String
  , from :: String
  , amount :: Int
  } deriving (Show, Read, Eq, Generic)

instance ToJSON TransactionArgs
instance FromJSON TransactionArgs

calculateTransactionHash :: String -> String -> Int -> Int -> String
calculateTransactionHash s r v t = hashString (concat [s, r, show v, show t])

timeStampTransaction :: (MonadIO m) => TransactionArgs -> m Transaction
timeStampTransaction args = do
  time <- liftIO epoch
  let hashid = calculateTransactionHash (to args) (from args) (amount args) time
  let transaction = Transaction
                    { tid            = hashid
                    , sender        = to args
                    , receiver      = from args
                    , value         = amount args
                    , timeProcessed = time
                    }
  return transaction

allTransactions :: [Block] -> [Transaction]
allTransactions chain = flatten [ transactions | Block _ _ _ transactions _ _  <- chain]

filterBySender :: String -> Transaction -> Bool
filterBySender key (Transaction _ sender _ _ _ ) | sender == key = True
filterBySender _ _ = False

filterByReceiver :: String -> Transaction -> Bool
filterByReceiver key (Transaction _ _ receiver _ _ ) | receiver == key = True
filterByReceiver _ _ = False

filterById :: String -> Transaction -> Bool
filterById key (Transaction id _ _ _ _ ) | id == key = True
filterById _ _ = False

credit :: String -> [Transaction] -> Int
credit key transactions = foldl (\acc (Transaction _ _ _ amount _)  -> acc + amount ) 0 $ filter (filterBySender key) transactions

debit :: String -> [Transaction] -> Int
debit key transactions = foldl (\acc (Transaction _ _ _ amount _)  -> acc + amount ) 0 $ filter (filterByReceiver key) transactions

balance :: String -> [Transaction] -> Int
balance key transaction = debit key transaction - credit key transaction

isNotOverDraft :: Transaction -> [Block] -> Bool
isNotOverDraft transaction chain = (balance (sender transaction) (allTransactions chain)) - (value transaction) >= 0

isValidTransaction :: Transaction -> [Block] -> Bool
isValidTransaction transaction chain
  | isNotOverDraft transaction chain = True
  | otherwise = False

isNewTransaction :: Transaction -> [Transaction] -> Bool
isNewTransaction transaction transactions
  | filter (filterById (tid transaction)) transactions == [] = True
  | otherwise = False

-- TODO: Need to refactor out
----------------------------------

data Node = Node { name      :: String
                 , uuid      :: Int
                 , address   :: String
                 } deriving (Generic, Show, Read, Eq)

instance ToJSON Node
instance FromJSON Node

data NodeArgs = NodeArgs { nName    :: String
                         , nAddress :: String
                         } deriving (Show, Eq, Generic)

instance ToJSON NodeArgs
instance FromJSON NodeArgs

initialNode :: Node
initialNode = Node "master" 0 "localhost:1234"

createNode :: (MonadIO m) => Node -> NodeArgs -> m Node
createNode lastNode nodeArgs = do
  let node = Node { name    = nName nodeArgs
                  , uuid    = uuid lastNode + 1
                  , address = nAddress nodeArgs
                  }
  return node

-- TODO: Need to refactor out
-- Block Logic (mostly) from github/legion
----------------------------------
-- LEGION: Structure of Block --
data Block = Block { index        :: Int
                   , previousHash :: String
                   , timestamp    :: Int
                   , blockData    :: [Transaction]
                   , nonce        :: Int
                   , blockHash    :: String
                   } deriving (Show, Read, Eq, Generic)

instance ToJSON Block
instance FromJSON Block
instance Binary Block

-- LEGION: Implementation of Block creation, hashing, proof of work, and validation --

calculateBlockHash :: Block -> String
calculateBlockHash (Block i p t b n _) = hashString (concat [show i, p, show t, show b, show n])

setBlockHash :: Block -> Block
setBlockHash block = block {blockHash = calculateBlockHash block}

difficultyTarget :: Integer
difficultyTarget = 0x0000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

satisfiesPow :: String -> Bool
satisfiesPow bHash =
  maybe
    (error $ "Something is wrong with the provided hash: " ++ bHash)
    (< difficultyTarget)
    (readMaybe ("0x" ++ bHash) :: Maybe Integer)

findNonce :: Block -> Int
findNonce block = do
  let bHash = calculateBlockHash block
      currentNonce = nonce block
  if satisfiesPow bHash
    then currentNonce
    else findNonce (block {nonce = currentNonce + 1})

setNonceAndHash :: Block -> Block
setNonceAndHash block = setBlockHash (block {nonce = findNonce block})

mineBlock :: (MonadIO m) => Block -> [Transaction] -> m Block
mineBlock lastBlock latestTransactions = do
  time <- liftIO epoch
  let block = Block { index        = index lastBlock + 1
                    , previousHash = blockHash lastBlock
                    , timestamp    = time
                    , blockData    = latestTransactions
                    , nonce        = 0
                    , blockHash    = "will be changed"
                    }
  return (setNonceAndHash block)

isValidNewBlock :: Block -> Block -> Bool
isValidNewBlock prev next
  | index prev + 1 == index next &&
    blockHash prev == previousHash next &&
    blockHash next == calculateBlockHash next &&
    satisfiesPow (blockHash next) = True
  | otherwise = False

isValidChain :: [Block] -> Bool
isValidChain chain = case chain of
  [] -> True
  [x] -> x == initialBlock
  (x:xs) ->
    let blockPairs = zip chain xs in
      x == initialBlock &&
      all (uncurry isValidNewBlock) blockPairs

initialBlock :: Block
initialBlock = do
  let block = Block 0 "0" 0 [Transaction "0" "God" "Corey" 1000 0] 0 ""
  setNonceAndHash block
