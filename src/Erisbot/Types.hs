{-# LANGUAGE FlexibleContexts, TemplateHaskell, OverloadedStrings, 
             ConstraintKinds, ScopedTypeVariables #-}
module Erisbot.Types where

import Network.IRC.ByteString.Parser
import Network (PortNumber)

import Control.Lens
import Data.ByteString.Char8 as BS
import Data.HashMap.Strict as HashMap

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Applicative
import Control.Exception.Lifted
import Data.Monoid
import Data.Maybe
import Data.String

import Control.Concurrent.Lifted
import System.Mem.Weak
import System.IO


data BotConf = 
  BotConf { network :: String
          , port :: PortNumber
          , nick :: String
          , user :: String
          , realname :: String
          , cmdPrefixes :: String
          , channels :: [String]
          , dataDir :: String
          }


data CommandData = CommandData { _cmdChannel   :: ByteString
                               , _cmdUserInfo  :: UserInfo
                               , _cmdParams    :: ByteString
                               , _cmdParamList :: [ByteString]
                               }
                   
data BotState = BotState { _botConf :: MVar BotConf
                         , _outQueue   :: Chan IRCMsg
                         , _inQueue    :: Chan IRCMsg
                         , _chanLocks :: MVar (HashMap ByteString (MVar ThreadId))
                         , _cmdHandlers :: MVar (HashMap ByteString (CommandHandler ()))
                         , _plugins :: MVar (HashMap ByteString PluginState)
                         , _currentChanLock :: Maybe (MVar ThreadId)
                         , _currentPlugin :: Maybe Plugin
                         , _debugLock :: MVar ThreadId
                         }


data Plugin = 
  Plugin { pluginName :: String
         , commands :: [(String, CommandHandler ())]
         , inputListeners :: [InputListener]
         , onLoad :: Bot ()
         , onUnload :: Bot ()
         }
           
               
data PluginState =
  PluginState { pluginConf    :: Plugin 
              , pluginThreads :: [Weak ThreadId] 
              }


-- |convenient typeclass synonym
type BotMonad bot = (MonadIO bot, MonadState BotState bot
                    , Functor bot, MonadBaseControl IO bot)

-- |base bot monad
type Bot = StateT BotState IO
-- |monad transformer used with 'withChannel'
type ChannelWriter = ReaderT ByteString


type InputListener = IRCMsg -> Bot ()

-- |monad trasnformer for command handlers
type CommandHandler = ReaderT CommandData Bot

makeLenses ''CommandData
makeLenses ''BotState


newBotState :: MonadIO io => BotConf -> io BotState
newBotState conf = liftIO $ do
  confVar <- newMVar conf
  outQ  <- newChan
  inQ   <- newChan
  chanL <- newMVar HashMap.empty
  cmdH <- newMVar HashMap.empty
  plugs <- newMVar HashMap.empty
  debugL <- newEmptyMVar
  return $ BotState confVar outQ inQ chanL cmdH plugs Nothing Nothing debugL 

copyBotState :: BotMonad bot => bot BotState
copyBotState = do
  confVar <- use botConf
  inQ' <- dupChan =<< use inQueue 
  outQ' <- dupChan =<< use outQueue
  chanL' <- use chanLocks
  cmdH' <- use cmdHandlers
  plugs <- use plugins
  currentPlug <- use currentPlugin
  debugL' <- use debugLock
  return $ BotState confVar outQ' inQ' chanL' cmdH' plugs Nothing currentPlug 
                    debugL'

runBot :: BotState -> Bot a -> IO a
runBot = flip evalStateT
  
forkBot :: BotMonad bot => bot () -> bot ThreadId
forkBot bot = do
  botState' <- copyBotState
  --forkFinally (set botState' >> bot) finalizer
  fork (put botState' >> bot >> finalizer undefined)
  where
    finalizer _ = do
      maybe (return ()) (void . liftIO . tryTakeMVar) =<< use currentChanLock
      
  
forkBot_ :: BotMonad bot => bot () -> bot ()
forkBot_ = void . forkBot
  

runInputListener :: InputListener -> Bot a
runInputListener listener = 
  forever . handle exHandler $ listener =<< recvMsg
  where
    exHandler (e :: SomeException) = debugMsg (show e)

forkInputListener :: InputListener -> Bot ThreadId
forkInputListener = forkBot . runInputListener

forkInputListener_ :: InputListener -> Bot ()
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
  result <- runReaderT cWriter channel `finally` unlockChannel
  currentChanLock .= Nothing -- needed because finally discards state changes
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
    let outputThread = do
          putMVar lock threadId
          System.IO.hPutStrLn stderr $ show threadId <> ": " <> msg
        finalizer _ = void . liftIO $ takeMVar lock
    --forkFinally outputThread finalizer
    void $ fork ( outputThread >> finalizer undefined)
  
debugMsgByteString :: BotMonad bot => ByteString -> bot ()
debugMsgByteString = debugMsg . BS.unpack

isCmdPrefix :: Char -> Bot Bool
isCmdPrefix c = use botConf 
                >>= readMVar
                >>= return . (c `Prelude.elem`) . cmdPrefixes