-- | Module: Handlers
-- Description: Handlers process an arbitrary connection over a TLS socket
--
-- A Handler is a monad transformer stack over IO storing the socket it is handling
-- as well as a write-only log for various actions.  This file defines the handler
-- type, it's associated log type, and functions to run/evaluate handlers as well as
-- various basic handlers to do simple actions over the socket.  These include sending
-- ByteStrings, files, receiving data, and logging information.
--
-- Handlers do not actually write their log to anything.  Instead, the log, a list of Text values,
-- is returned after evaluation to allow it to be printed with arbitrary formatting and to multiple places.  

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Handlers 
    (
        Handler (..),
        Connection,
        Log,
        runHandler,
        evalHandler,
        -- ** Networking Utility Handlers
        send, sendAll, sendFile, recv, echo,
        -- ** Logging Utility
        appLog, logAddr, logSock
    )
    where

import Network.Socket (Socket, SockAddr) 
import qualified Network.Socket.ByteString as BS

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS 

import qualified Data.ByteString.Lazy as BSL
import qualified Network.Socket.ByteString.Lazy as BSL

import Data.Text (Text, pack)

import Control.Monad.Reader
import Control.Monad.Writer


-- | Logs are simply lists of text.  They are kept as lists of text instead of a
-- single Text value to allow for formatting after the execution of the Handler, instead of during.  
-- This decouples how your log is formatted from the logging itself and allows for logging to
-- different outputs (stdout, files, etc) to be formatted differently.
type Log = [Text]
-- | A socket connection is represented by a tuple of the socket we are sending over
-- and the receiving address (IPv6 for compatibility).  
type Connection = (Socket, SockAddr)
-- | A handler is a monad that abstracts away the handling of one client.
-- It abstracts away storing the connection info, a log if needed,
-- and allows IO to enable send and receive abilities.
newtype Handler a = Handler (ReaderT Connection (WriterT Log (IO)) a)
    deriving (Functor, 
              Applicative, 
              Monad, 
              MonadIO, 
              MonadReader Connection,
              MonadWriter Log)

-- | Runs a handler and returns its value and log as a tuple.  
-- In most cases, the return value of a handled connection is irrelevant so evalHandler is preferred.
runHandler :: Handler a -> Connection -> IO (a, Log)
runHandler (Handler h) conn = runWriterT $ runReaderT h conn

-- | Runs a handler and ignores its result, returning only the log.
evalHandler :: Handler a -> Connection -> IO Log
evalHandler h c = snd <$> runHandler h c

-- Handler constructs
-- These are simple handlers that do small tasks that can be composed
-- More complex tasks will likely be in another file

-- | Sends part of a ByteString to the associated socket and
-- returns the actual number of bytes sent.  Note that, since
-- not all the data is necessarily sent at once, [length bytes >= sent]!
-- In general, unless this behavior is desired, use sendAll.
send :: ByteString -> Handler Int
send bytes = do
    (socket, _) <- ask
    sent <- liftIO $ BS.send socket bytes 
    pure sent

-- | Sends an entire ByteString, ensuring that it is sent in full.  
-- Doing so is delegated to the network library.
sendAll :: ByteString -> Handler ()
sendAll bytes = do
    (socket, _) <- ask
    liftIO $ BS.sendAll socket bytes

-- | Sends a file directly over the associated socket, given the program has permissions to do so.  
-- If the program cannot read the file, this will fail in the same manner as readFile would.  
--
-- This uses a lazy bytestring to avoid having the entire file be resident in memory at once.
sendFile :: FilePath -> Handler ()
sendFile path = do
    (socket, _) <- ask
    liftIO $ BSL.readFile path >>= BSL.sendAll socket

-- | Receives up to n bytes over the associated socket.
-- The function returns an empty bytestring if the connection is closed.
recv :: Int -> Handler ByteString
recv amt = do
    (socket, _) <- ask
    liftIO $ BS.recv socket amt

-- | Simply reads all bytes sent on the connection and repeats (echos)
-- them back to the user.
echo :: Handler ()
echo = do
    val <- recv 4096
    sendAll val
    if BS.null val then pure () else echo 

-- | A modified tell to properly append a single value to the log, 
-- as the Log type is a list of Text values.  
--
-- This function is simply @tell . pure@.
appLog :: Text -> Handler ()
appLog = tell . pure
    

-- | Adds the address to the internal log.
logAddr :: Handler ()
logAddr = do
    (_, addr) <- ask
    appLog $ pack . show $ addr
    -- tell $ ["Connection from: " ++ show addr] -- old formatting

-- | Logs the socket to the internal log.  
logSock :: Handler ()
logSock = do
    (sock, _) <- ask
    appLog $ "On socket: " <> (pack . show) sock
