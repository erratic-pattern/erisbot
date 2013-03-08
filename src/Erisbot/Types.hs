{-# LANGUAGE FlexibleContexts, TemplateHaskell, OverloadedStrings, 
             ConstraintKinds, ScopedTypeVariables #-}
module Erisbot.Types where

import Network.IRC.ByteString.Parser

import Control.Lens
import Data.ByteString.Char8 as BS
import Data.HashMap.Strict as HashMap

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Applicative
import Control.Exception.Lifted
import Data.Monoid
import Data.Maybe

import Control.Concurrent
import Data.String
import System.IO

data CommandData = CommandData { _cmdChannel   :: ByteString
                               , _cmdUserInfo  :: UserInfo
                               , _cmdParams    :: ByteString
                               , _cmdParamList :: [ByteString]
                               }
                   
data BotState = BotState { _outQueue   :: Chan IRCMsg
                         , _inQueue    :: Chan IRCMsg
                         , _chanLocks :: MVar (HashMap ByteString (MVar ThreadId))
                         , _cmdHandlers :: MVar (HashMap ByteString (CommandHandler ()))
                         , _currentChanLock :: Maybe (MVar ThreadId)
                         , _debugLock :: MVar ThreadId
                         }


-- |convenient typeclass synonym
type BotMonad bot = (MonadIO bot, MonadState BotState bot, Functor bot)

-- |base bot monad
type Bot = StateT BotState IO
-- |monad transformer used with 'withChannel'
type ChannelWriter = ReaderT ByteString

-- |monad trasnformer for command handlers
type CommandHandler = ReaderT CommandData Bot

makeLenses ''CommandData
makeLenses ''BotState


emptyBotState :: MonadIO io => io BotState
emptyBotState = liftIO $ do
  outQ  <- newChan
  inQ   <- newChan
  chanL <- newMVar HashMap.empty
  cmdH <- newMVar HashMap.empty
  debugL <- newEmptyMVar
  return $ BotState outQ inQ chanL cmdH Nothing debugL

copyBotState :: BotMonad bot => bot BotState
copyBotState = do
  inQ' <- liftIO . dupChan =<< use inQueue 
  outQ' <- liftIO . dupChan =<< use outQueue
  chanL' <- use chanLocks
  cmdH' <- use cmdHandlers
  debugL' <- use debugLock
  return $ BotState outQ' inQ' chanL' cmdH' Nothing debugL'

runBot :: BotState -> Bot a -> IO a
runBot = flip evalStateT
  
forkBot :: BotMonad bot => Bot () -> bot ThreadId
forkBot bot = do
  botState' <- copyBotState
  liftIO . forkIO $ runBot botState' bot
  
forkBot_ :: BotMonad bot => Bot () -> bot ()
forkBot_ = void . forkBot
  

forkInputListener :: (IRCMsg -> Bot ()) -> Bot ThreadId
forkInputListener listener = do
  forkBot . forever $ handle exHandler . listener =<< recvMsg
  where
    exHandler (e :: SomeException) = debugMsg (show e)

forkInputListener_ :: (IRCMsg -> Bot ()) -> Bot ()
forkInputListener_ = void . forkInputListener

runCommandHandler :: CommandData -> CommandHandler a -> Bot a
runCommandHandler = flip runReaderT


sendMsg :: BotMonad bot => ByteString -> [ByteString] -> ByteString -> bot ()
sendMsg cmd params trail = do
  outQ <- use outQueue
  let msg = ircMsg cmd params trail
  debugMsg $ "Sending message to output queue: " 
    <> fromString (show msg)
  liftIO . writeChan outQ $ ircMsg cmd params trail

recvMsg :: BotMonad bot => bot IRCMsg
recvMsg = liftIO . readChan =<< use inQueue

say :: BotMonad bot => ByteString -> ChannelWriter bot ()
say chanMsg = do 
  channel <- ask
  sendMsg "PRIVMSG" [channel] chanMsg

emote :: BotMonad bot => ByteString -> ChannelWriter bot ()
emote chanMsg = do
  channel <- ask
  sendMsg "PRIVMSG" [channel] $ "\x01\&ACTION " <> chanMsg <> "\x01"


lockChannel :: BotMonad bot => ByteString -> bot ()
lockChannel channel = do
  isLocking <- isJust <$> use currentChanLock
  when isLocking $ error "Thread is already locking a channel"
    
  lockMapVar <- use chanLocks
  lockMap <- liftIO $ takeMVar lockMapVar
  let mChanLock = HashMap.lookup channel lockMap
  chanLock <- 
    liftIO $ case mChanLock of
      Just lock -> do
        putMVar lockMapVar lockMap
        return lock   
      Nothing -> do  
        lock <- newEmptyMVar
        putMVar lockMapVar (HashMap.insert channel lock lockMap)
        return lock
  liftIO $ putMVar chanLock =<< myThreadId
  currentChanLock .= Just chanLock

unlockChannel :: BotMonad bot => bot ()
unlockChannel = do 
  mLock <- use currentChanLock
  case mLock of
    Just lock -> do
      liftIO . void . tryTakeMVar $ lock
      currentChanLock .= Nothing
    Nothing ->
      return ()

withChannel :: BotMonad bot => ByteString -> ChannelWriter bot a -> bot a
withChannel channel cWriter = do
  lockChannel channel
  result <- runReaderT cWriter channel
  unlockChannel
  return result

replyToChannel :: ChannelWriter CommandHandler a -> CommandHandler a
replyToChannel writer = do
  channel <- view cmdChannel
  withChannel channel writer


debugMsg :: BotMonad bot => String -> bot ()
debugMsg msg = do
  lock <- use debugLock
  liftIO $ do
    threadId <- myThreadId
    putMVar lock threadId
    System.IO.hPutStrLn stderr $ show threadId <> ": " <> msg
    void (takeMVar lock)
  
debugMsgByteString :: BotMonad bot => ByteString -> bot ()
debugMsgByteString = debugMsg . BS.unpack