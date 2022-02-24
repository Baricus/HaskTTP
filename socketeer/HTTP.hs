{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: HTTP
-- Description: HTTP related Handlers for sending/receiving HTTP messages
--
-- This module defines higher level Handlers for dealing directly with HTTP messages.  
-- These mainly revolve around sending and receiving whole HTTP messages.  

module HTTP (
    -- * Data Types from HTTP.Types
    HTTP (..), Method (..), StartLine (..), Message (..), Sendable (..), Target (..),
    -- * HTTP Handlers
    -- ** Sending/receiving messages
    recvHttp, sendMessage, 
    filterFailed,
    -- ** Sending status codes
    sendStat, send200, send400, send404,
    -- *** Logging variants
    sendStatL, send200L, send400L, send404L,
    -- ** Logging
    logReq
    ) where

import HTTP.Types
import HTTP.Parsing
import Sendable

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Result, IResult (..), parseWith)

import qualified Data.Text as T

import Handlers (Handler (..), recv, sendAll, appLog)

import TextShow

import Data.Map (Map)
import qualified Data.Map as M

-- | 
-- Converts a handler that takes in an HTTP message and leftover bytes to one that handles
-- all unseccesful parses by returning a 400 (invalid request) error code.
filterFailed :: ((Message, ByteString) -> Handler a) -> (Result Message -> Handler ())
-- this should be impossible unless the client connection died
filterFailed _ (Partial _) = send400 
filterFailed _ (Fail _ _ _) = send400 
filterFailed hdlr (Done r msg) = hdlr (msg, r) *> pure ()

-- | 
-- Parses a request or response message using the attoparsec parsers defined in HTTP.Parsing.
recvHttp :: Handler (Result Message)
recvHttp = parseWith (recv 4096) message BS.empty

-- | 
-- Takes in a map of headers and the status code and sends the entire thing over the wire.  
-- If an invalid code is given, this function does nothing.  
sendStat :: Map ByteString ByteString -> Int -> Handler ()
sendStat m c | c > 99 && c < 512 = sendMessage $ Message (StatLine (HTTP 1 1) c) m
             | otherwise = pure ()

-- send status codes with no headers (good for when headers are simple enough to not worry)

-- | 400 Bad Request
send400 :: Handler ()
send400 = sendStat M.empty 400

-- | 404 Not Found
send404 :: Handler ()
send404 = sendStat M.empty 404

-- | 200 OK
send200 :: Handler ()
send200 = sendStat M.empty 200

-- | Identical to sendStat but logs the status code sent to the client.
sendStatL :: Map ByteString ByteString -> Int -> Handler ()
sendStatL m c = appLog (showt c) *> sendStat m c

-- | Log enabled 400 Bad Request
send400L :: Handler ()
send400L = sendStatL M.empty 400

-- | Log enabled 404 Bad Request
send404L :: Handler ()
send404L = sendStatL M.empty 404

-- | Log enabled 200 OK
send200L :: Handler ()
send200L = sendStatL M.empty 200


-- | sendMessage
-- sends a HTTP message down the wire, excluding the body
sendMessage :: Message -> Handler ()
sendMessage m = sendAll $ packBytes m

-- | logReq
-- logs an HTTP message method, path, and version for a request
logReq :: Message -> Handler ()
logReq Message{start = (ReqLine m (Target f "") v)} = appLog $ showt m <> " " <> T.pack f <> " " <> showt v
logReq Message{start = (ReqLine m (Target f args) v)} = appLog $ showt m <> " " <> T.pack f <> "?" <> args <> " " <> showt v
logReq _ = appLog "Not a request"
