{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Server where

import           Lib
import           Control.Monad                    (forever)
import           Control.Monad.Trans
import           Control.Concurrent.Async
import           Data.IORef
import           System.Log.Logger
import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics     hiding (to, from)

type Api = SpockM () MySession BlockChainState ()

type ApiAction a = SpockAction () () () a

data MySession = EmptySession

data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                       , nodeState :: IORef [Node]
                                       , transactionState :: IORef [Transaction]
                                       } deriving (Generic)

addDebug :: (MonadIO m) => String -> m ()
addDebug str = liftIO (debugM "312Coin" (show str))

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

getBlockChain :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Block]
getBlockChain = do
  (BlockChainState chain _ _) <- getState
  liftIO $ readIORef chain

getLatestBlock :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m Block
getLatestBlock = fmap last getBlockChain

addBlock :: MonadIO m => IORef [Block] -> Block -> m ()
addBlock ref block = do
  chain <- liftIO $ readIORef ref
  if isValidNewBlock (last chain) block
    then do
      addDebug "adding new block"
      _ <- liftIO $ atomicModifyIORef' ref $ \b -> (b ++ [block], b ++ [block])
      return ()
    else
      addDebug "new block not valid. skipping"

getNodes :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Node]
getNodes = do
  (BlockChainState _ nodes _) <- getState
  liftIO $ readIORef nodes

registerNode :: MonadIO m => IORef [Node] -> Node -> m ()
registerNode ref node = do
  _ <- liftIO $ atomicModifyIORef' ref $ \n -> (n ++ [node], n ++ [node])
  return ()

getLatestNode :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m Node
getLatestNode = fmap last getNodes

getNodeId :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => NodeArgs -> m Node
getNodeId nodeArgs = do
  latestNode <- getLatestNode
  createNode latestNode nodeArgs

getTransactions :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Transaction]
getTransactions = do
  (BlockChainState _ _ transactions) <- getState
  liftIO $ readIORef transactions

addTransaction :: MonadIO m => IORef [Transaction] -> Transaction -> m ()
addTransaction ref transaction = do
  addDebug "Adding new transaction"
  _ <- liftIO $ atomicModifyIORef' ref $ \t -> (t ++ [transaction], t ++ [transaction])
  return ()

grabTransactions :: MonadIO m => IORef [Transaction] -> m [Transaction]
grabTransactions ref = do
  addDebug "Grabbing transactions"
  transactions <- liftIO $ atomicModifyIORef' ref $ \t -> ([], t)
  return transactions

app :: Api
app = do
  -- Block routes
  get "block" $ do
    (BlockChainState chain _ transactions) <- getState
    lastBlock <- getLatestBlock
    lastTransactions <- grabTransactions transactions
    block <- mineBlock lastBlock lastTransactions
    _ <- addBlock chain block
    chain <- getBlockChain
    addDebug $ show chain
    json $ chain
  --  Chain routes
  get "chain" $ do
    chain <- getBlockChain
    addDebug $ show chain
    json $ chain
  --  Node routes
  get "node" $ do
    nodes <- getNodes
    addDebug $ show nodes
    json $ nodes
  post "node" $ do
    (BlockChainState _ ref _) <- getState
    (nodeArgs :: NodeArgs) <- jsonBody'
    (newNode :: Node) <- getNodeId nodeArgs
    _ <- registerNode ref newNode
    nodes <- getNodes
    addDebug $ show nodes
    json $ nodes
  --  Transaction routes
  get "transaction" $ do
    transactions <- getTransactions
    addDebug $ show transactions
    json $ transactions
  -- TODO: Need to refactor out the creating transaction logic into Lib
  post "transaction" $ do
    (args :: TransactionArgs) <- jsonBody'
    addDebug $ show args
    (BlockChainState _ _ ref) <- getState
    transaction <- timeStampTransaction args
    _ <- addTransaction ref transaction
    transactions <- getTransactions
    addDebug $ show transactions
    json $ transactions

runServer :: IO ()
runServer = do
  addDebug "Starting Server"
  -- TODO: I dont think we need these maybe statements, most of it can just be inititialized usually
  chainRef <- maybe (newIORef [initialBlock]) (const $ newIORef []) (Nothing)
  nodeRef <- maybe (newIORef [initialNode]) (const $ newIORef []) (Nothing)
  transactionRef <- maybe (newIORef []) (const $ newIORef []) (Nothing)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChainState chainRef nodeRef transactionRef)
  runSpock 8080 (spock spockCfg app)
  addDebug "Server Running on 8080"
