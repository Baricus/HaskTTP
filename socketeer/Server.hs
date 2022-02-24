{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Server
-- Description: Servers for TCP socket Handlers
--
-- This module defines a single and multithreaded server to be used
-- with handlers to handle client connections.  

module Server (allowedQueue, runServer, runMultiServer) where

import Data.Word (Word16)
import Network.Socket
import Control.Monad (forever)
import Control.Exception (bracket, SomeException (..))
import Control.Concurrent (forkFinally, forkIO, ThreadId)

import qualified Data.Text as T

import Control.Concurrent.STM

import Handlers
import Loggers

-- | number of connections that can wait simultaneously to be accepted
allowedQueue :: Word16
allowedQueue = 10


-- | Creates an IP socket which can connect to either v4 or v6.
newIpSock :: IO Socket
newIpSock = do
    sock <- socket AF_INET6 Stream defaultProtocol
    setSocketOption sock ReuseAddr 1 
    withFdSocket sock setCloseOnExecIfNeeded -- just in case someone forks the process
    pure sock

-- | Returns a socket ready to accept connections
-- given a port number and number of people to wait on.
newListener :: Word16 -> Word16 -> IO Socket
newListener portNum waitNum = do
    sock <- newIpSock
    infos <- getAddrInfo (Just $ defaultHints {addrFamily = AF_INET6, addrFlags = [AI_PASSIVE]}) 
                         Nothing 
                         (Just $ show portNum)
    let name = (addrAddress . head) infos
    bind sock name
    listen sock $ fromIntegral waitNum
    pure sock

-- | Runs a server on the supplied port with the supplied connection handler.
-- This is not multithreaded--it handles one connection at a time--but 
-- the function ensures the socket for each client is always closed after being handled.
--
-- As this is single threaded, it can only allow a single client to connect at once;
-- all others will wait to be accepted by the single thread.  As such, minimizing the
-- time handling each user is extremely important.  
runServer :: Word16 -> Handler a -> Logger -> IO ()
runServer port handler logger =  bracket (newListener port allowedQueue) 
                                 close 
                                 (\sock -> forever $ accept sock >>= wrapper)
    -- wraps the handler to ensure we always close the socket
    where wrapper conn@(csock, _) = do
              logged <- evalHandler handler conn
              writeLog logger logged
              close csock -- close to ensure immediate return 


-- | 
-- The body of a thread dedicated solely to logging, reading from a
-- shared queue and writing it using the supplied logger.
loggerThread :: TBQueue Log -> Logger -> IO ()
loggerThread q l = forever $ (atomically $ readTBQueue q) >>= (writeLog l)

-- | 
-- A wrapper for handling 1 connection, split from the full server for clarity.  
handleOne :: Handler a -> TBQueue Log -> Connection -> IO ()
handleOne h q c@(sock, _) = do
    line <- evalHandler h c
    gracefulClose sock 250 -- close the socket before logging
    atomically $ writeTBQueue q line

-- | 
-- A function to simply call handleOne in a forked thread, ensuring that
-- the socket is always closed regardless of errors.  
-- 
-- On an exception, the exception is written to the log instead of the log value
-- for debugging purposes.  
forkWrapper :: Handler a -> TBQueue Log -> Connection -> IO ThreadId
forkWrapper h q conn@(sock, _) = do
    -- ensure even if the handler crashes the socket closes
    -- and writes a thread crash in the log as well
    forkFinally (handleOne h q conn) (either 
                        (\(SomeException e) -> close sock *> 
                            (atomically $ writeTBQueue q ["ERROR: " <> (T.pack . show) e])) 
                        (const $ pure ()))

-- |runMultiServer runs a multithreaded server instance, forking a thread for each client.  
-- This forked thread runs the supplied handler code and then passes the resulting log over to a dedicated
-- logging thread to prevent multiple simultaneous writes.  
--
-- The port used is supplied as the first argument, the handler to process each connection the second,
-- and the logger to output the logs as the third.  
--
-- This function will run until interrupted.  
runMultiServer :: Word16 -> Handler a -> Logger -> IO ()
runMultiServer port handl loggr = do
    q <- atomically $ newTBQueue 25
    _ <- forkIO $ loggerThread q loggr -- we don't care about the thread id, Ctrl-C will kill it
    bracket 
        -- open the listening port
        (newListener port allowedQueue) 
        close -- the listening port
        -- handle a connection in a new thread
        (\sock -> forever $ accept sock >>= forkWrapper handl q)
