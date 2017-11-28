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

type ApiAction a = SpockAction () () () a

data MySession = EmptySession

data BlockChainState = BlockChainState { blockChainState :: IORef [Block]
                                       , nodeState :: IORef [Node]
                                       } deriving (Generic)

addDebug :: (MonadIO m) => String -> m ()
addDebug str = liftIO (debugM "312Coin" (show str))

getBlockChain :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Block]
getBlockChain = do
  (BlockChainState chain _) <- getState
  liftIO $ readIORef chain

getNodes :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => m [Node]
getNodes = do
  (BlockChainState _ nodes) <- getState
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

-- createNode :: (SpockState m ~ BlockChainState, MonadIO m, HasSpock m) => String -> m Node
-- createNode stringData = do
--   nodeArgs <- json $ stringData
--   show
--   mineBlockFrom lastBlock stringData


app :: SpockM () MySession BlockChainState ()
app = do
  get "block" $ do
    json $ Block 0 "0" 0 "initial data" 0 ""
  get "chain" $ do
    chain <- getBlockChain
    json $ chain
  get "node" $ do
    node <- getNodes
    json $ node
  post "node" $ do
    (BlockChainState ref currentNodes) <- getState
    (nodeArgs :: NodeArgs) <- jsonBody'
    (newNode :: Node) <- getNodeId nodeArgs
    _ <- registerNode currentNodes newNode
    json $ newNode

runServer :: IO ()
runServer = do
  addDebug "Starting Server"
  chainRef <- maybe (newIORef [initialBlock]) (const $ newIORef []) (Nothing)
  nodeRef <- maybe (newIORef [initialNode]) (const $ newIORef []) (Nothing)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (BlockChainState chainRef nodeRef )
  runSpock 8080 (spock spockCfg app)
  addDebug "Server Running on 8080"
