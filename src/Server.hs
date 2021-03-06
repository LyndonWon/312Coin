{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- LEGION CODE SOURCED FROM: https://github.com/aviaviavi/legion/blob/master/src/Server.hs --
module Server where

-- LEGION: Package selection --
import           Lib
import           Control.Monad                    (forever, when)
import           Control.Monad.Trans
import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Data.IORef
import           Data.Maybe
import           System.Log.Logger
import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics     hiding (to, from)

import qualified Control.Distributed.Backend.P2P  as P2P
import qualified Data.Binary                      as B
import qualified Data.Text                        as T

type Api = SpockM () MySession BlockChainState ()

type ApiAction a = SpockAction () () () a

-- LEGION: Arguments for CLI --
data CliArgs = CliArgs { httpPort :: String
                         , p2pPort  :: String
                         , seedNode :: Maybe String
                         }

data MySession = EmptySession

-- LEGION: Basic structure for BlockChainState --
-- 312Coin: additional fields added --
data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                       , nodeState :: IORef [Node]
                                       , transactionState :: IORef [Transaction]
                                       , node            :: LocalNode
                                       , pid             :: ProcessId
                                       } deriving (Generic)

-- LEGION: Basic types for BlockUpdate --
-- 312Coin: additional types added --
data BlockUpdate =  UpdateChain Block
                  | ReplaceChain [Block]
                  | RequestChain
                  | UpdateTransactionPool Transaction
                  | ReplaceTransactionPool [Transaction]
                  | RequestTransactionPool deriving (Generic)

instance B.Binary BlockUpdate

p2pServiceName :: String
p2pServiceName = "blockchain-update-service"

addDebug :: (MonadIO m) => String -> m ()
addDebug str = liftIO (debugM "312Coin" (show str))

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

-- LEGION: Basic implementation of getBlockChain, getLatestBlock and addBlock --
getBlockChain :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Block]
getBlockChain = do
  (BlockChainState chain _ _ _ _) <- getState
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
  (BlockChainState _ nodes _ _ _) <- getState
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
  addDebug "Getting transactions in chain"
  (BlockChainState ref _ _ _ _) <- getState
  chain <- liftIO $ readIORef ref
  let allTransactions = flatten [ transactions | Block _ _ _ transactions _ _  <- chain]
  return allTransactions

getCurrentTransactions :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Transaction]
getCurrentTransactions = do
  addDebug "Getting transactions not in chain."
  (BlockChainState _ _ transactions _ _) <- getState
  liftIO $ readIORef transactions

addNewTransaction :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => Transaction -> m ()
addNewTransaction transaction = do
  (BlockChainState chainRef _ transactionsRef _ _) <- getState
  chain <- liftIO $ readIORef chainRef
  if isValidTransaction transaction chain
    then do
      addDebug "Adding new transaction."
      _ <- liftIO $ atomicModifyIORef' transactionsRef $ \t -> (t ++ [transaction], t ++ [transaction])
      return ()
    else
      addDebug "Transaction not valid."

addTransaction :: MonadIO m => IORef [Transaction] -> Transaction -> m ()
addTransaction ref transaction = do
  transactions <- liftIO $ readIORef ref
  if isNewTransaction transaction transactions
    then do
    addDebug "Adding new transaction to pool from peer"
    _ <- liftIO $ atomicModifyIORef' ref $ \t -> (t ++ [transaction], t ++ [transaction])
    return ()
  else
    addDebug "Already have transaction."

requestTransactionPool :: MonadIO m => LocalNode -> m ()
requestTransactionPool localNode = liftIO $ runProcess localNode $ do
  addDebug "Requesting transaction pool."
  P2P.nsendPeers p2pServiceName RequestTransactionPool

replaceTransactionPool :: MonadIO m => IORef [Transaction] -> [Transaction] -> m ()
replaceTransactionPool transactionRef newPool = do
  setPool <- liftIO $ atomicModifyIORef' transactionRef $ const (newPool, newPool)
  addDebug ("Updated transaction pool: " ++ show setPool)

sendTransactionPool :: MonadIO m => LocalNode -> IORef [Transaction] -> m ()
sendTransactionPool localNode transactionRef = liftIO $ runProcess localNode $ do
  addDebug "Emitting transaction pool."
  transactions <- liftIO $ readIORef transactionRef
  P2P.nsendPeers p2pServiceName $ ReplaceTransactionPool transactions

-- LEGION: Implementation of replaceChain, requestChain, sendChain --
-- Chain Functions --
replaceChain :: MonadIO m => IORef [Block] -> [Block] -> m ()
replaceChain chainRef newChain = do
  currentChain <- liftIO $ readIORef chainRef
  if (not . isValidChain) newChain || length currentChain >= length newChain
    then addDebug $ "chain is not valid for updating!: " ++ show newChain
    else do
      setChain <- liftIO $ atomicModifyIORef' chainRef $ const (newChain, newChain)
      addDebug ("Updated chain: " ++ show setChain)

requestChain :: MonadIO m => LocalNode -> m ()
requestChain localNode = liftIO $ runProcess localNode $ do
  addDebug "Requesting chain."
  P2P.nsendPeers p2pServiceName RequestChain

sendChain :: MonadIO m => LocalNode -> IORef [Block] -> m ()
sendChain localNode chainRef = liftIO $ runProcess localNode $ do
  addDebug "Emitting chain."
  newChain <- liftIO $ readIORef chainRef
  P2P.nsendPeers p2pServiceName $ ReplaceChain newChain

app :: Api
app = do
  -- LEGION: Basic GET /block and GET /chain implementation --
  -- 312Coin: Additional functionality for storing transactions --
  -- Block routes
  -- TODO: Refactor this out into its own controller method
  get "block" $ do
    (BlockChainState chain _ transactions localNode _) <- getState
    lastBlock <- getLatestBlock
    lastTransactions <- liftIO $ atomicModifyIORef' transactions $ \t -> ([], t)
    block <- mineBlock lastBlock lastTransactions
    _ <- addBlock chain block
    liftIO $ runProcess localNode $ P2P.nsendPeers p2pServiceName $ UpdateChain block
    chain <- getBlockChain
    addDebug $ show chain
    json $ chain
  get "chain" $ do
    chain <- getBlockChain
    addDebug $ show chain
    json $ chain
  get "node" $ do
    nodes <- getNodes
    addDebug $ show nodes
    json $ nodes
  post "node" $ do
    (BlockChainState _ ref _ _ _) <- getState
    (nodeArgs :: NodeArgs) <- jsonBody'
    (newNode :: Node) <- getNodeId nodeArgs
    _ <- registerNode ref newNode
    nodes <- getNodes
    addDebug $ show nodes
    json $ nodes
  get "transaction" $ do
    transactions <- getCurrentTransactions
    addDebug $ show transactions
    json $ transactions
  get "chain/transactions" $ do
    transactions <- getAllTransactions
    addDebug $ show transactions
    json $ transactions
  -- TODO: Refactor this out into its own controller method
  post "transaction" $ do
    (BlockChainState _ _ _ localNode _) <- getState
    (args :: TransactionArgs) <- jsonBody'
    addDebug $ show args
    transaction <- timeStampTransaction args
    _ <- addNewTransaction transaction
    liftIO $ runProcess localNode $ P2P.nsendPeers p2pServiceName $ UpdateTransactionPool transaction
    json $ transaction

-- LEGION: Basic P2P Messaging for updating BlockChainState --
-- 312Coin: Additional functionality added for transaction consensus --
runP2P port bootstrapNode = P2P.bootstrapNonBlocking "localhost" port (maybeToList $ P2P.makeNodeId `fmap` bootstrapNode) initRemoteTable

runServer :: CliArgs -> IO ()
runServer args = do
  addDebug "Starting REST API and P2P Node."
  (localNode, procId) <- runP2P (p2pPort args) (seedNode args) (return ())
  chainRef <- maybe (newIORef [initialBlock]) (const $ newIORef []) (seedNode args)
  nodeRef <- maybe (newIORef [initialNode]) (const $ newIORef []) (seedNode args)
  transactionRef <- maybe (newIORef []) (const $ newIORef []) (seedNode args)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChainState chainRef nodeRef transactionRef localNode procId)
  _ <- async $ runSpock (read (httpPort args) :: Int) (spock spockCfg Server.app)
  addDebug $ "REST API Running on " ++ (httpPort args)
  runProcess localNode $ do
    getSelfPid >>= register p2pServiceName
    liftIO $ threadDelay 1000000
    _ <- if isJust $ seedNode args
    then do
      addDebug "This is not the initial node, getting chain from P2P seed."
      requestChain localNode
      requestTransactionPool localNode
    else addDebug "This is the initial node, not requesting a chain."
    addDebug $ "P2P Node Running on: " ++ (p2pPort args)
    forever $ do
      message <- expect :: Process BlockUpdate
      addDebug "Message received."
      case message of
        (ReplaceChain chain) -> do
          addDebug $ "Replace data with new chain: " ++ show chain
          replaceChain chainRef chain
        (UpdateChain block) -> do
          addDebug $ "Add new block to chain: " ++ show block
          addBlock chainRef block
          replaceTransactionPool transactionRef []
        RequestChain -> do
          addDebug "Request for current chain."
          sendChain localNode chainRef
        (ReplaceTransactionPool transactions) -> do
          addDebug $ "Replace transactions with new transaction pool: " ++ show transactions
          replaceTransactionPool transactionRef transactions
        (UpdateTransactionPool transaction) -> do
          addDebug $ "Add new transaction to pool: " ++ show transaction
          addTransaction transactionRef transaction
        RequestTransactionPool -> do
          addDebug "Providing current transaction pool."
          sendTransactionPool localNode transactionRef
