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
      addDebug "Adding new block"
      _ <- liftIO $ atomicModifyIORef' ref $ \b -> (b ++ [block], b ++ [block])
      return ()
    else
      addDebug "New block not valid. skipping"

getNodes :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Node]
getNodes = do
  addDebug "Getting all registered nodes"
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

getAllTransactions :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Transaction]
getAllTransactions = do
  addDebug "Getting current transactions in chain"
  (BlockChainState ref _ _) <- getState
  chain <- liftIO $ readIORef ref
  let allTransactions = flatten [ transactions | Block _ _ _ transactions _ _  <- chain]
  return allTransactions

getCurrentTransactions :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Transaction]
getCurrentTransactions = do
  addDebug "Getting current transactions not in chain"
  (BlockChainState _ _ transactions) <- getState
  liftIO $ readIORef transactions

addTransaction :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => Transaction -> m ()
addTransaction transaction = do
  (BlockChainState chainRef _ transactionsRef) <- getState
  chain <- liftIO $ readIORef chainRef
  if isValidNewTransaction transaction chain
    then do
      addDebug "Adding new transaction"
      _ <- liftIO $ atomicModifyIORef' transactionsRef $ \t -> (t ++ [transaction], t ++ [transaction])
      return ()
    else
      addDebug "Transaction not valid. skipping"

app :: Api
app = do
  -- Block routes
  get "block" $ do
    (BlockChainState chain _ transactions) <- getState
    lastBlock <- getLatestBlock
    lastTransactions <- liftIO $ atomicModifyIORef' transactions $ \t -> ([], t)
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
    transactions <- getCurrentTransactions
    addDebug $ show transactions
    json $ transactions
  get "chain/transactions" $ do
    transactions <- getAllTransactions
    addDebug $ show transactions
    json $ transactions
  -- TODO: Need to refactor out the creating transaction logic into Lib
  post "transaction" $ do
    (args :: TransactionArgs) <- jsonBody'
    addDebug $ show args
    transaction <- timeStampTransaction args
    _ <- addTransaction transaction
    transactions <- getCurrentTransactions
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
