{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, ParallelListComp #-}

module Lib where

import System.IO.Unsafe
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

-- TODO: Need to refactor out Transaction logic to transaction.hs
----------------------------------
data Transaction = Transaction
  { sender :: String
  , receiver :: String
  , value :: Int
  , timeProcessed :: Int
  } deriving (Show, Read, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

data TransactionArgs = TransactionArgs
  { to :: String
  , from :: String
  , amount :: Int
  } deriving (Show, Read, Eq, Generic)

instance ToJSON TransactionArgs
instance FromJSON TransactionArgs

timeStampTransaction :: (MonadIO m) => TransactionArgs -> m Transaction
timeStampTransaction args = do
  time <- liftIO epoch
  let transaction = Transaction
                    { sender        = to args
                    , receiver      = from args
                    , value    = amount args
                    , timeProcessed = time
                    }
  return transaction

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

allTransactions :: [Block] -> [Transaction]
allTransactions chain = flatten [ transactions | Block _ _ _ transactions _ _  <- chain]

filterBySender :: String -> Transaction -> Bool
filterBySender key (Transaction sender _ _ _ ) | sender == key = True
filterBySender _ _ = False

filterByReceiver :: String -> Transaction -> Bool
filterByReceiver key (Transaction _ receiver _ _ ) | receiver == key = True
filterByReceiver _ _ = False

credit :: String -> [Transaction] -> Int
credit key transactions = foldl (\acc (Transaction _ _ amount _)  -> acc + amount ) 0 $ filter (filterBySender key) transactions

debit :: String -> [Transaction] -> Int
debit key transactions = foldl (\acc (Transaction _ _ amount _)  -> acc + amount ) 0 $ filter (filterByReceiver key) transactions

balance :: String -> [Transaction] -> Int
balance key transaction = debit key transaction - credit key transaction

isNotOverDraft :: Transaction -> [Block] -> Bool
isNotOverDraft transaction chain = (balance (sender transaction) (allTransactions chain)) - (value transaction) >= 0

isValidNewTransaction :: Transaction -> [Block] -> Bool
isValidNewTransaction transaction chain
  | isNotOverDraft transaction chain = True
  | otherwise = False


-- TODO: Need to refactor out Node logic to node.hs
----------------------------------

data Node = Node
  { name :: String
  , id  :: Int
  } deriving (Show, Read, Eq, Generic)

instance ToJSON Node
instance FromJSON Node

initialNode :: Node
initialNode = Node "master" 1

----------------------------------
data Block = Block { index        :: Int
                   , previousHash :: String
                   , timestamp    :: Int
                   , blockData    :: [Transaction]
                   , nonce        :: Int
                   , blockHash    :: String
                   } deriving (Show, Read, Eq, Generic)

instance ToJSON Block
instance FromJSON Block

epoch :: IO Int
epoch = round `fmap` getPOSIXTime

sha256 :: String -> Maybe (Digest SHA256)
sha256 = digestFromByteString . hash . pack

hashString :: String -> String
hashString = maybe (error "Something went wrong generating a hash") show . sha256

calculateBlockHash :: Block -> String
calculateBlockHash (Block i p t b n _) = hashString (concat [show i, p, show t, show b, show n])

setBlockHash :: Block -> Block
setBlockHash block = block {blockHash = calculateBlockHash block}

setNonceAndHash :: Block -> Block
setNonceAndHash block = setBlockHash (block {nonce = findNonce block})

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

initialBlock :: Block
initialBlock = do
  let block = Block 0 "0" 0 [Transaction "God" "Corey" 1000 0] 0 ""
  setNonceAndHash block

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
