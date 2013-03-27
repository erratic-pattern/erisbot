{-# LANGUAGE OverloadedStrings, FlexibleContexts, NamedFieldPuns
           , RecordWildCards #-}
module Erisbot where
import Erisbot.Types
import Erisbot.Commands
import Erisbot.Plugins
import Erisbot.Plugins.URL
import Erisbot.Plugins.Sed
import Network
import Network.IRC.ByteString.Parser
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec
import Data.Monoid ((<>))
import Data.Char (toLower)
import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import System.IO

--waitForNoIdent :: Handle -> IO ()
--waitForNoIdent sock = do
--  line <- BS.hGetLine sock
--  BS.putStrLn line
--  if "No Ident response" `BS.isInfixOf` line
--    then return ()
--    else waitForNoIdent sock  

-- |Waits for 001 numeric before performing the given action
waitFor001 :: Bot s () -> Bot s ()
waitFor001 action = do
  msg <- recvMsg
  case msg of
    IRCMsg {msgCmd = "001"} -> action
    _ -> waitFor001 action

-- |Indefinitely read input from the given 'Handle', parse each line as an IRC message, and send the IRC message to the bot input queue
socketReader :: Handle -> Bot s a
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
        writeChan inQ msg
  
-- |Indefinitely read IRC messages from the bot output queue, serialize them, and write them to the given handle
socketWriter :: Handle -> Bot s a
socketWriter sock = do
  outQ <- use outQueue
  debugMsg "starting socketWriter"
  forever $ do
    debugMsg "waiting for output"
    line <- readChan outQ
    let msgStr = fromIRCMsg line
    debugMsgByteString msgStr
    liftIO (BS.hPutStr sock msgStr)
    debugMsg "output sent"

-- |Listens for PING messages and replies with a suitable PONG
pingListener :: InputListener s
pingListener IRCMsg {msgCmd = "PING", msgParams, msgTrail} 
  = sendMsg "PONG" msgParams msgTrail 
pingListener _ = return ()
    
rejoinListener :: InputListener s
rejoinListener IRCMsg {msgCmd = "KICK", msgParams = [channel, nick]} = do
  BotConf{nick = myNick} <- readMVar =<< use botConf
  when (BS.map toLower nick == BS.map toLower (BS.pack myNick)) $ do
    threadDelay 3000000
    sendMsg "JOIN" [channel] ""

rejoinListener _ = return ()


-- |proof of concept
annoyingNickChanger :: Bot () ()
annoyingNickChanger = forever $ do
  threadDelay 2760000000 -- wait 46 minutes
  changeNick "erisbot"
  threadDelay 2760000000 -- wait 46 minutes
  changeNick "strifebot"
  where
    changeNick n = do
      use botConf >>= (`modifyMVar_` (\c -> return c {nick = n}))
      sendMsg "NICK" [BS.pack n] ""
  

erisbot :: BotConf -> IO ()
erisbot conf@BotConf {..} = withSocketsDo $ do
  sock <- connectTo network (PortNumber port)
  putStrLn $ "Connecting to " ++ network ++ ":" ++ show port ++ "..."
  hSetBuffering sock LineBuffering
  botState <- newBotState conf ()
  runBot botState $ do
    debugMode .= True
    debugMsg "initializing socket reader"
    forkBot_ (socketReader sock)
    debugMsg "initializing socket writer"
    forkBot_ (socketWriter sock)
    debugMsg "initializing command dispatcher"
    forkInputListener_ commandDispatcher
    forkInputListener_ pingListener
    forkInputListener_ urlListener
    forkInputListener_ rejoinListener
    forkBot_ annoyingNickChanger
    forkInputListenerWithState_ HM.empty sedListener
    addCommand defaultCommand { name =  "say"
                              , handler = sayCommand
                              }
    threadDelay 2000000
    sendMsg "NICK" [BS.pack nick] ""
    sendMsg "USER" [BS.pack user, "0", "0"] (BS.pack realname)
    waitFor001 $ do
      forM_ plugins (uncurry loadPlugin)
      forM_ channels $ \c ->
        sendMsg "JOIN" [BS.pack c] ""
    debugMsg "main sleeping"
    sleepForever
  
  where sleepForever = threadDelay maxBound >> sleepForever