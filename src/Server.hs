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
import           GHC.Generics

type Api = SpockM () MySession BlockChainState ()

type ApiAction a = SpockAction () () () a

data MySession = EmptySession

data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                         , nodeState :: IORef [Node]
                                         , transactionState :: IORef [Transaction]
                                       } deriving (Generic)

addDebug :: (MonadIO m) => String -> m ()
addDebug str = liftIO (debugM "312Coin" (show str))

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

app :: Api
app = do
  get "block" $ do
    json $ Block 0 "0" 0 "initial data" 0 ""
  get "chain" $ do
    chain <- getBlockChain
    json $ chain
  get "node" $ do
    nodes <- getNodes
    json $ nodes
  get "transaction" $ do
    transactions <- getTransactions
    json $ transactions
  post "transaction" $ do
    (transaction :: TransactionArgs) <- jsonBody'
    json $ transaction

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

runServer :: IO ()
runServer = do
  addDebug "Starting Server"
  chainRef <- maybe (newIORef [initialBlock]) (const $ newIORef []) (Nothing)
  nodeRef <- maybe (newIORef [initialNode]) (const $ newIORef []) (Nothing)
  transactionRef <- maybe (newIORef []) (const $ newIORef []) (Nothing)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChainState chainRef nodeRef transactionRef)
  runSpock 8080 (spock spockCfg app)
  addDebug "Server Running on 8080"
