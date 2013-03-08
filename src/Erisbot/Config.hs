{-# LANGUAGE OverloadedStrings #-}
module Erisbot.Config where
import Data.ByteString.Char8 (ByteString)
import Network (HostName, PortNumber)

network :: HostName
network = ""

port :: PortNumber
port = 6667

nick, user, realname :: ByteString
nick = ""
user = ""
realname = ""


-- |List of valid command prefixes
cmdPrefixes :: [Char]
cmdPrefixes = ""

channels :: [ByteString]
channels = []
