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

getNodes :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Node]
getNodes = do
  (BlockChainState _ nodes _) <- getState
  liftIO $ readIORef nodes

getTransactions :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Transaction]
getTransactions = do
  (BlockChainState _ _ transactions) <- getState
  liftIO $ readIORef transactions

addTransaction :: MonadIO m => IORef [Transaction] -> Transaction -> m ()
addTransaction ref transaction = do
  addDebug "Adding new transaction"
  _ <- liftIO $ atomicModifyIORef' ref $ \t -> (t ++ [transaction], t ++ [transaction])
  return ()

app :: Api
app = do
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
