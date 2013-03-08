{-# LANGUAGE OverloadedStrings, FlexibleContexts, NamedFieldPuns #-}
module Erisbot where
import Erisbot.Config
import Erisbot.Types
import Erisbot.Commands
import Erisbot.Plugins.Url

import Network
import Network.IRC.ByteString.Parser
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec
import Data.Monoid
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import System.IO

cmdPrefixes :: [Char]
cmdPrefixes = "!"

--waitForNoIdent :: Handle -> IO ()
--waitForNoIdent sock = do
--  line <- BS.hGetLine sock
--  BS.putStrLn line
--  if "No Ident response" `BS.isInfixOf` line
--    then return ()
--    else waitForNoIdent sock  

-- |Waits for 001 numeric before performing the given action
waitFor001 :: Bot () -> Bot ()
waitFor001 action = do
  msg <- recvMsg
  case msg of
    IRCMsg {msgCmd = "001"} -> action
    _ -> waitFor001 action

-- |Indefinitely read input from the given 'Handle', parse each line as an IRC message, and send the IRC message to the bot input queue
socketReader :: Handle -> Bot a
socketReader sock = do
  inQ <- use inQueue
  debugMsg "starting socketReader"
  forever $ do
    debugMsg "waiting for socket input..."
    line <- liftIO (BS.hGetLine sock)
    debugMsgByteString line
    case feed (toIRCMsg line) "" of
      Partial _ -> debugMsg "impossible partial result"
      Fail _ context errMsg ->
        debugMsg $ "parse error: " <> show context <> " " <> errMsg 
      Done leftover msg -> do
        debugMsg $ "Sending message to input queue: " <> show msg
        --putStrLn . show $ msg
        --unless (BS.null leftover) . BS.hPutStrLn stderr 
        --  $ "socketReader: warning: leftovers when parsing: " <> leftover
        liftIO (writeChan inQ msg)
  
-- |Indefinitely read IRC messages from the bot output queue, serialize them, and write them to the given handle
socketWriter :: Handle -> Bot a
socketWriter sock = do
  outQ <- use outQueue
  debugMsg "starting socketWriter"
  forever $ do
    debugMsg "waiting for output"
    line <- liftIO (readChan outQ)
    let msgStr = fromIRCMsg line
    debugMsgByteString msgStr
    liftIO (BS.hPutStr sock msgStr)
    debugMsg "output sent"

-- |Listens for PING messages and replies with a suitable PONG
pingListener :: IRCMsg -> Bot ()
pingListener IRCMsg {msgCmd = "PING", msgParams, msgTrail} 
  = sendMsg "PONG" msgParams msgTrail 
pingListener _ = return ()
    
main :: IO ()
main = withSocketsDo $ do
  sock <- connectTo network (PortNumber port)
  putStrLn $ "Connecting to " ++ network ++ ":" ++ show port ++ "..."
  hSetBuffering sock LineBuffering
  botState <- emptyBotState
  runBot botState $ do
    forkBot_ (socketReader sock)
    forkBot_ (socketWriter sock)
    forkInputListener_ commandDispatcher
    forkInputListener_ pingListener
    forkInputListener_ urlListener
    addCommand "say" sayCommand
    sendMsg "NICK" [nick] ""
    sendMsg "USER" [user, "*", "*"] realname
    waitFor001 $ do
      forM_ channels $ \c ->
        sendMsg "JOIN" [c] ""
    debugMsg "main sleeping"
    sleepForever
  
  where sleepForever = liftIO (threadDelay maxBound) >> sleepForever
