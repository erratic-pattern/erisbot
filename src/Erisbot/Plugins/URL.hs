{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Erisbot.Plugins.URL where
import Erisbot.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception.Lifted
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SBS
import Data.Attoparsec.Char8
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isAlphaNum)
import Data.Maybe
import Network.IRC.ByteString.Parser
import Network.HTTP.Conduit
import Text.HTML.DOM
import Text.XML.Cursor
import Prelude hiding (takeWhile)

infixr 1 <<>>
(<<>>) :: Parser ByteString -> Parser ByteString -> Parser ByteString
(<<>>) = liftM2 SBS.append

opt :: Parser ByteString -> Parser ByteString
opt p = fromMaybe "" <$> optional p

between :: Int -> Int -> Parser ByteString -> Parser ByteString
between min max p
  | max < 1 = return ""
  | min < 1 = do
    mResult <- optional p
    case mResult of
      Just result -> return result <<>> between min (max-1) p
      Nothing -> return ""
  | otherwise = p <<>> between (min-1) (max-1) p

exactly :: Int -> Parser ByteString -> Parser ByteString
exactly n = between n n

concatMany :: Parser ByteString -> Parser ByteString
concatMany p = SBS.concat <$> many p

concatMany1 :: Parser ByteString -> Parser ByteString
concatMany1 p = SBS.concat <$> many1 p

protocol = ("http" <<>> opt (string "s") <|> "ftp") <<>> string "://"

ipDigit = between 1 3 (SBS.singleton <$> satisfy isDigit)

ipAddress = exactly 3 (ipDigit <<>> string ".") <<>> ipDigit

hostName = SBS.intercalate "." <$> takeWhile1 isAlphaNum `sepBy1` string "."

pathPart = (string "/" <|> string "?") 
           <<>> opt (takeWhile (not . isEndChar))
  where                
    isEndChar c = isSpace c || inClass ",;:\">)]}" c

urlParser = protocol <<>> (ipAddress <|> hostName) <<>> opt pathPart

scanForURLs :: ByteString -> [ByteString]
scanForURLs "" = []
scanForURLs str = 
  case feed (parse urlParser str) "" of
    Done leftover url -> url : scanForURLs leftover
    Fail _ _ _ -> let Just (_, nextStr) = SBS.uncons str
                  in scanForURLs nextStr
    _  -> error "scanForURL: impossible error"
    


-- |Largely borrowed from the strict bytestring vaiant on Hackage
breakSubstring :: LBS.ByteString -> LBS.ByteString 
                  -> (LBS.ByteString, LBS.ByteString)
breakSubstring pat src = search 0 src
  where
    search n s
      | LBS.null s             = (src,LBS.empty)      -- not found
      | pat `LBS.isPrefixOf` s = (LBS.take n src,s)
      | otherwise              = let Just (_, sTail) = LBS.uncons s
                                 in search (n+1) sTail


isRedundant :: ByteString -> ByteString -> Bool
isRedundant _ _ = False

urlListener :: InputListener s
urlListener IRCMsg {msgCmd = "PRIVMSG", msgParams = [channel], msgTrail = msg} = do
  debugMsg "URL listener active"
  let urls = scanForURLs msg
  titles <- fmap catMaybes . forM urls $ \url -> do
    debugMsgByteString url
    handle exceptionHandler . liftIO . withManager $ \m -> do
      req <- liftIO $ parseUrl (SBS.unpack url)
      res <- httpLbs req m
      return $ case lookup "Content-Type" (responseHeaders res) of
        Nothing -> Nothing
        Just contType
          | "text/html" `SBS.isInfixOf` contType -> 
            if not (SBS.null title || isRedundant title url)
            then Just title
            else Nothing
          | otherwise -> Nothing
          where
            doc = fromDocument . parseLBS $ responseBody res
            titleContent = doc $// laxElement "title" &/ content
            title = case titleContent of
              (t:_) -> encodeUtf8 t
              _     -> ""
              
  unless (null titles) . withChannel channel . say 
    $ SBS.intercalate " | " titles
    
    where
      exceptionHandler (e :: SomeException) = do
        debugMsg (show e)
        return Nothing

urlListener _ = return ()
  
plugin = 
  defaultPlugin {
    onLoad = forkInputListener_ urlListener
  }