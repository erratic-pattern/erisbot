{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Erisbot.Commands where
import Erisbot.Types
import Network.IRC.ByteString.Parser
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict as HashMap
import Data.Char
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Concurrent
import Erisbot.Types


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
        cmds <- liftIO . readMVar =<< use cmdHandlers
        case HashMap.lookup cmdName cmds of
          Just (CommandState cmdState cmdHandler) ->
            forkBot_ () . runCommandHandler cmdData cmdState $ cmdHandler
          Nothing -> do
            withChannel channel . say $ "Invalid command: " <> cmdName

      where
        (cmdName, paramsWithLeadingSpace) = BS.break isSpace cmdMsg
        params = BS.dropWhile isSpace paramsWithLeadingSpace
        paramList = BS.words params
        cmdData = CommandData channel userInfo params paramList
    _ -> return ()
    
-- |Adds a command handler to the bot, overwriting any previous handler with the same name.
addCommand :: ByteString -> s -> CommandHandler s () -> Bot s' ()
addCommand cName cState cHandler = do
  cHandlersVar <- use cmdHandlers
  liftIO $ modifyMVar_ cHandlersVar 
    (return . HashMap.insert cName (CommandState cState cHandler))


sayCommand :: CommandHandler s ()
sayCommand = do
  msg <- view cmdParams
  replyToChannel (say msg)
