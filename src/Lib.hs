{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Lib where

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

----------------------------------
data Transaction = Transaction
  { sender :: String
  , receiver :: String
  , value :: Int
  , timestamp :: Int
  } deriving (Generic, Show)

instance ToJSON Transaction
instance FromJSON Transaction

data TransactionArgs = TransactionArgs
  { to :: String
  , from :: String
  , amount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TransactionArgs
instance FromJSON TransactionArgs

timestampTransaction :: (MonadIO m) => TransactionArgs -> m Transaction
timestampTransaction args = do
  time <- liftIO epoch
  let transaction = Transaction
                    { sender        = to args
                    , receiver      = from args
                    , value    = amount args
                    , timestamp = time
                    }
  return (liftIO transaction)

-- addTransaction :: (MonadIO m) => Transaction -> String -> m Transaction
-- addTransaction stringData = do
--   time <- liftIO epoch
--   let transaction = Transaction
--                     { to = stringData
--                     , from = stringData
--                     , amount = stringData
--                     , timestamp = time
--                     }
--   return (transaction)

----------------------------------

data Node = Node
  { name :: String
  , id  :: Int
  } deriving (Generic, Show, Read, Eq)

instance ToJSON Node
instance FromJSON Node

initialNode :: Node
initialNode = Node "master" 1

----------------------------------

data Block = Block { index        :: Int
                   , previousHash :: String
                   , timestamp    :: Int
                   , blockData    :: String
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
calculateBlockHash (Block i p t b n _) = hashString (concat [show i, p, show t, b, show n])

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
  let block = Block 0 "0" 0 "initial data" 0 ""
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

mineBlockFrom :: (MonadIO m) => Block -> String -> m Block
mineBlockFrom lastBlock stringData = do
  time <- liftIO epoch
  let block = Block { index        = index lastBlock + 1
                    , previousHash = blockHash lastBlock
                    , timestamp    = time
                    , blockData    = stringData
                    , nonce        = 0
                    , blockHash    = "will be changed"
                    }
  return (setNonceAndHash block)
