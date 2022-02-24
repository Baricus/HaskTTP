{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server
import Handlers
import Loggers
import HTTP
import Utils

import Data.ByteString (ByteString)

-- handle 1 request (a Message and the corresponding ByteString)
-- subfunctions are implemented in Utils.hs
processRequest :: (Message, ByteString) -> Handler ()
processRequest pair = do
    logReq . fst $ pair
    -- all 3 do nothing if it's not the proper request type
    handleGet pair
    handleHead pair
    handleUnimplemented pair

-- launches the server on a port and runs until interrupted
main :: IO ()
main = runMultiServer 8000 
            (logAddr *> recvHttp >>= filterFailed processRequest)
            (toStdOut [" -- ", " : "] <> toFileP "log.txt") 
