{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Erisbot.Plugins.Sed where
import Erisbot.Types
import Network.IRC.ByteString.Parser
import Control.Monad
import Control.Applicative
import Control.Lens ((%=))
import Data.Sequence as Seq
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict as HashMap
import Data.Char
import Data.List
import System.Process

type History = Seq IRCMsg
type HistoryMap = HashMap BS.ByteString History

historySize = 100

trimHistory :: Int -> History -> History
trimHistory size h = 
  case viewr h of
    EmptyR -> h
    h' :> _
      | Seq.length h > size -> trimHistory size h'
      | otherwise           -> h

sedListener :: InputListener HistoryMap
sedListener
    msg@IRCMsg {msgCmd = "PRIVMSG", msgParams = [channel], msgTrail = trail} = do
      let msgStr = dropWhile isSpace . BS.unpack $ trail
      case parseSed msgStr of
        Nothing -> return ()
        Just (pat, repl, wholeSed) -> do
          debugMsg $ "pattern: " ++ pat
          debugMsg $ "replace: " ++ repl
          debugMsg $ "whole: " ++ wholeSed
      localState %= HashMap.insertWith updateHistory channel newHistory
      where
        newHistory = Seq.singleton msg
        updateHistory _ h = trimHistory historySize (msg  <| h)
        
sedListener _ = return ()


parseSed :: String -> Maybe (String, String, String)
parseSed ('s' : delim : str)
  | delim /= '\\' && not (isSpace delim) && not (isAlpha delim)
    = let (pat, rest) = parsePattern False delim str
          mRepl = parseReplacement False delim rest
      in case mRepl of
        Nothing -> Nothing
        Just repl -> 
          let wholeExpr = 's' : delim : pat ++ delim : repl
          in Just (pat, repl, wholeExpr)
  
  where    
    parsePattern _ _ [] = ([], [])
    parsePattern isEscaped delim ('\\':ss) 
      = let (pat, rest) = parsePattern (not isEscaped) delim ss
        in ('\\':pat, rest)
    parsePattern isEscaped delim (s:ss)
      | s == delim && not isEscaped = ([], ss)
      | otherwise = let (pat, rest) = parsePattern False delim ss
                    in (s : pat, rest)
                       
    parseReplacement _ _ [] = Nothing
    parseReplacement isEscaped delim ('\\':ss) =
      ('\\' :) <$> parseReplacement (not isEscaped) delim ss
    parseReplacement isEscaped delim (s:ss)
      | s == delim && not isEscaped = Just []
      | otherwise = (s :) <$> parseReplacement False delim ss
         

parseSed _ = Nothing
