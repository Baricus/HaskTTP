{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Server
import Handlers
import Loggers
import HTTP
import Utils

import Data.ByteString (ByteString)
import System.Environment (getArgs, getProgName)
import Data.Word (Word16)
import Text.Read (readMaybe)

-- either returns the default port for no arguments,
-- the requested port, or Nothing if it could not be parsed
getPort :: Word16 -> IO (Maybe Word16)
getPort def = getArgs >>= (\l -> case l of
                      []       -> pure . pure $ def
                      (pStr:_) -> pure $ readMaybe pStr
                    )

-- handle 1 request (a Message and the corresponding ByteString)
-- subfunctions are implemented in Utils.hs
processRequest :: (Message, ByteString) -> Handler ()
processRequest pair = do
    logReq . fst $ pair
    -- all 3 do nothing if it's not the proper request type
    handleGet pair
    handleHead pair
    handleUnimplemented pair

-- launches the server on a port and runs until interrupted (default port 8000)
main :: IO ()
main = getPort 8000 >>= (\case 
            Nothing -> putStr "Usage: " >> (getProgName >>= putStr) >> putStrLn " [port]"
            Just p  -> putStr "Listening on port " >> print p >>
                runMultiServer p
                    (logAddr *> recvHttp >>= filterFailed processRequest)
                    (toStdOut [" -- ", " : "] <> toFileP "log.txt") 
        )
