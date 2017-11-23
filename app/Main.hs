module Main where

import Lib
import Server
import           System.Environment               (getArgs)
import           System.IO                        (stdout)
import           System.Log.Formatter
import           System.Log.Handler               (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

initLogger :: IO ()
initLogger = let logPriority = DEBUG
                 format lh = return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  in
    streamHandler stdout logPriority >>= format >>= \s ->
      fileHandler ("312Coin.log") logPriority >>= format >>= \h ->
        updateGlobalLogger rootLoggerName $ setLevel logPriority . setHandlers [s, h]


main :: IO ()
main = do
  _ <- initLogger
  runServer
