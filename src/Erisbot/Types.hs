{-# LANGUAGE FlexibleContexts, TemplateHaskell, OverloadedStrings, 
             ConstraintKinds, ScopedTypeVariables,
             ExistentialQuantification #-}
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
import System.Plugins.Load (Module)
import System.IO
import Prelude hiding (catch)


type PluginName = String

data BotConf = 
  BotConf { network :: String
          , port :: PortNumber
          , nick :: String
          , user :: String
          , realname :: String
          , cmdPrefixes :: String
          , channels :: [String]
          , dataDir :: FilePath
          , pluginDir :: FilePath
          , pluginIncludeDirs :: [FilePath]
          , plugins :: [(PluginName, FilePath)]
          }

defaultBotConf :: BotConf
defaultBotConf = BotConf { network = confErr "network address"
                         , port = confErr "port number"
                         , nick = confErr "nick name"
                         , user = confErr "user name"
                         , realname = confErr "real name"
                         , cmdPrefixes = "!"
                         , channels = []
                         , dataDir = confErr "data directory"
                         , pluginDir = confErr "plugin directory"
                         , pluginIncludeDirs = []
                         , plugins = []
                         }
  where
    confErr field = error $ "No " ++ field ++ " specified in bot configuration"


data CommandData = CommandData { _cmdChannel   :: ByteString
                               , _cmdUserInfo  :: UserInfo
                               , _cmdParams    :: ByteString
                               , _cmdParamList :: [ByteString]
                               }
                   
data BotState s = 
  BotState { _botConf :: MVar BotConf
           , _outQueue :: Chan IRCMsg
           , _inQueue  :: Chan IRCMsg
           , _chanLocks :: MVar (HashMap ByteString (MVar ThreadId))
           , _cmdHandlers :: MVar (HashMap ByteString CommandState)
           , _pluginMap :: MVar (HashMap PluginName PluginState)
           , _currentChanLock :: Maybe (MVar ThreadId)
           , _currentPlugin :: Maybe Plugin
           , _debugMode :: Bool
           , _debugLock :: MVar ThreadId
           , _localState :: s
           }
  

data Plugin = Plugin { onLoad :: Bot () ()
                     , onUnload :: Bot () ()
                     }
               
defaultPlugin :: Plugin
defaultPlugin = Plugin (return ()) (return ())

data PluginState =
  PluginState { pluginData     :: Plugin
              , pluginModule   :: Module
              , pluginThreads  :: [Weak ThreadId] 
              }


-- |convenient typeclass synonym
type BotMonad s bot = ( MonadIO bot, MonadState (BotState s) bot
                      , Functor bot, MonadBaseControl IO bot)

-- |base bot monad
type Bot s = StateT (BotState s) IO

-- |monad transformer used with 'withChannel'
type ChannelWriterT = ReaderT ByteString


type InputListener s = IRCMsg -> Bot s ()

type CommandHandler s = ReaderT CommandData (Bot s)

data CommandState = forall s.
  CommandState { cmdState  :: s
               , cmdAction :: CommandHandler s ()
               , cmdLock   :: Maybe (MVar ThreadId)
               }

data Command s = Command { name :: ByteString
                         , state :: s
                         , handler :: CommandHandler s ()
                         , allowConcurrent :: Bool
                         }

defaultCommand :: Command ()
defaultCommand = Command (error "defaultCommand: No command name given")
                         ()
                         (return ())
                         True

makeLenses ''CommandData
makeLenses ''BotState


newBotState :: MonadIO io => BotConf -> s -> io (BotState s)
newBotState conf s = liftIO $ do
  confVar <- newMVar conf
  outQ <- newChan
  inQ <- newChan
  chanL <- newMVar HashMap.empty
  cmdH <- newMVar HashMap.empty
  plugs <- newMVar HashMap.empty
  debugL <- newEmptyMVar
  return $ BotState confVar outQ inQ chanL cmdH plugs Nothing Nothing 
                    False debugL s

copyBotState :: BotMonad s bot => s' -> bot (BotState s')
copyBotState s' = do
  confVar <- use botConf
  inQ' <- dupChan =<< use inQueue 
  outQ' <- dupChan =<< use outQueue
  chanL' <- use chanLocks
  cmdH' <- use cmdHandlers
  plugs <- use pluginMap
  currentPlug <- use currentPlugin
  debugMode' <- use debugMode
  debugL' <- use debugLock
  return $ BotState confVar outQ' inQ' chanL' cmdH' plugs Nothing currentPlug 
                    debugMode' debugL' s'

runBot :: BotState s -> Bot s a -> IO a
runBot = flip evalStateT

forkBot :: BotMonad s bot => Bot () () -> bot ThreadId
forkBot = forkBotWithState ()

forkBot_ :: BotMonad s bot => Bot () () -> bot ()
forkBot_ = forkBotWithState_ ()

forkBotWithState :: BotMonad s bot => 
                    s' -> Bot s' () -> bot ThreadId
forkBotWithState s' botAct = do
  botState' <- copyBotState s'
  liftIO . fork . runBot botState' $ botAct `catch` exHandler
  where
    exHandler (e :: SomeException) = do
      debugMsg (show e)
      throw e
      
  
forkBotWithState_ :: BotMonad s bot => s' -> Bot s' () -> bot ()
forkBotWithState_ s' = void . forkBotWithState s'

runInputListener :: InputListener s -> Bot s a
runInputListener listener = 
  forever $ listener =<< recvMsg
  where

forkInputListener :: InputListener () -> Bot () ThreadId
forkInputListener = forkInputListenerWithState ()

forkInputListener_ :: InputListener () -> Bot () ()
forkInputListener_ = forkInputListenerWithState_ ()

forkInputListenerWithState :: s' -> InputListener s' -> Bot s ThreadId
forkInputListenerWithState s' = forkBotWithState s' . runInputListener

forkInputListenerWithState_ :: s' -> InputListener s' -> Bot s ()
forkInputListenerWithState_ s' = void . forkInputListenerWithState s'

runCommandHandler :: CommandData -> CommandHandler s a -> Bot s a
runCommandHandler  = flip runReaderT


withLocalState :: s' -> Bot s' a -> Bot s a
withLocalState s bot = do
  s' <- copyBotState s
  liftIO $ runBot s' bot
  


sendMsg :: BotMonad s bot => 
           ByteString -> [ByteString] -> ByteString -> bot ()
sendMsg cmd params trail = do
  outQ <- use outQueue
  let msg = ircMsg cmd params trail
  debugMsg $ "Sending message to output queue: " <> fromString (show msg)
  liftIO . writeChan outQ $ msg

recvMsg :: BotMonad s bot => bot IRCMsg
recvMsg = liftIO . readChan =<< use inQueue

say :: BotMonad s bot => ByteString -> ChannelWriterT bot ()
say chanMsg = do 
  channel <- ask
  sendMsg "PRIVMSG" [channel] chanMsg

emote :: BotMonad s bot => ByteString -> ChannelWriterT bot ()
emote chanMsg = do
  channel <- ask
  sendMsg "PRIVMSG" [channel] $ "\x01\&ACTION " <> chanMsg <> "\x01"


lockChannel :: BotMonad s bot => ByteString -> bot ()
lockChannel channel = do
  isLocking <- isJust <$> use currentChanLock
  when isLocking $ error "Thread is already locking a channel"
    
  lockMapVar <- use chanLocks
  lockMap <- takeMVar lockMapVar
  let mChanLock = HashMap.lookup channel lockMap
  chanLock <- 
    case mChanLock of
      Just lock -> do
        putMVar lockMapVar lockMap
        return lock
      Nothing -> do  
        lock <- newEmptyMVar
        putMVar lockMapVar (HashMap.insert channel lock lockMap)
        return lock
  putMVar chanLock =<< myThreadId
  currentChanLock .= Just chanLock

unlockChannel :: BotMonad s bot => bot ()
unlockChannel = do 
  mLock <- use currentChanLock
  case mLock of
    Just lock -> do
      void . takeMVar $ lock
      currentChanLock .= Nothing
    Nothing ->
      return ()

withChannel :: BotMonad s bot => 
               ByteString -> ChannelWriterT bot a -> bot a
withChannel channel cWriter = do
  result <- bracket (lockChannel channel) (const unlockChannel) $
            (const $ runReaderT cWriter channel)
  currentChanLock .= Nothing -- needed because lifted bracket discards 
                             -- state changes in the finalizer
  return result

replyToChannel :: ChannelWriterT (CommandHandler s) a -> CommandHandler s a
replyToChannel writer = do
  channel <- view cmdChannel
  withChannel channel writer


debugMsg :: BotMonad s bot => String -> bot ()
debugMsg msg = do
  isDebugMode <- use debugMode
  when isDebugMode  $ do
    lock <- use debugLock
    threadId <- myThreadId
    forkBot_ 
      . bracket_ (putMVar lock threadId) (void $ takeMVar lock) $
      liftIO . System.IO.putStrLn $ show threadId <> ": " <> msg
  
debugMsgByteString :: BotMonad s bot => ByteString -> bot ()
debugMsgByteString = debugMsg . BS.unpack

isCmdPrefix :: Char -> Bot s Bool
isCmdPrefix c = use botConf 
                >>= readMVar
                >>= return . (c `Prelude.elem`) . cmdPrefixes