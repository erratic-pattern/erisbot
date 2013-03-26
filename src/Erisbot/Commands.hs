{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards #-}
module Erisbot.Commands where
import Erisbot.Types
import Network.IRC.ByteString.Parser
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict as HM
import Data.Char
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Lens
import Control.Concurrent.Lifted
import Control.Exception.Lifted


-- |Listens for bot commands and dispatches them to registered command handlers
commandDispatcher :: IRCMsg -> Bot s ()
commandDispatcher msg = do
  case msg of
    IRCMsg {msgPrefix = Just (Left userInfo)
           ,msgCmd = "PRIVMSG"
           ,msgParams = [channel]
           ,msgTrail = (BS.uncons -> Just (firstChar, cmdMsg))
           }
      -> do
      isPrefix <- isCmdPrefix firstChar
      when isPrefix $ do 
        cmdHandlersVar <- use cmdHandlers
        cmds <- liftIO . readMVar $ cmdHandlersVar
        case HM.lookup cmdName cmds of
          Just (CommandState s cmdHandler mCmdLock) ->
            forkBotWithState_ s . runCommandHandler cmdData $ do
              threadId <- myThreadId
              let (lockCmd, unlockCmd) =
                    case mCmdLock of
                      Just cmdLock -> (putMVar cmdLock threadId, 
                                       void (takeMVar cmdLock))
                      Nothing -> (return (), return ())
              bracket_ lockCmd unlockCmd $ do
                cmdHandler
                s' <- use localState
                modifyMVar_ cmdHandlersVar 
                  (return 
                   . HM.insert cmdName (CommandState s' cmdHandler mCmdLock))
          Nothing -> do
            withChannel channel . say $ "Invalid command: " <> cmdName

      where
        (cmdName, paramsWithLeadingSpace) = BS.break isSpace cmdMsg
        params = BS.dropWhile isSpace paramsWithLeadingSpace
        paramList = BS.words params
        cmdData = CommandData channel userInfo params paramList
    _ -> return ()
    
-- |Adds a command handler to the bot, overwriting any previous handler with the same name.
addCommand :: Command s -> Bot s' ()
addCommand Command{..} = do
  cHandlersVar <- use cmdHandlers
  lock <- if allowConcurrent 
          then return Nothing
          else Just <$> liftIO newEmptyMVar
  liftIO $ modifyMVar_ cHandlersVar 
    (return . HM.insert name (CommandState state handler lock))


sayCommand :: CommandHandler s ()
sayCommand = do
  msg <- view cmdParams
  replyToChannel (say msg)
