-- LEGION CODE SOURCED FROM: https://github.com/aviaviavi/legion/blob/master/app/Main.hs --
module Main where

import Lib
import Server
import           System.Environment               (getArgs)
import           System.IO                        (stdout)
import           System.Log.Formatter
import           System.Log.Handler               (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

initLogger :: String -> IO ()
initLogger port = let logPriority = DEBUG
                      format lh = return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  in
    streamHandler stdout logPriority >>= format >>= \s ->
      fileHandler ("312Coin" ++ port ++ ".log") logPriority >>= format >>= \h ->
        updateGlobalLogger rootLoggerName $ setLevel logPriority . setHandlers [s, h]

main :: IO ()
main = do
  args <- getArgs >>= \a -> case a of
        [h,p] -> return $ CliArgs h p Nothing
        [h,p,i] -> return $ CliArgs h p $ Just i
        _ -> fail "Usage: 312Coin httpPort :: String p2pPort :: String bootstrapAddress :: [String]\n"
  _ <- initLogger $ p2pPort args
  runServer args
