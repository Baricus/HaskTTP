{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Parsing
-- Description: An attoparsec-based parser for HTTP messages
--
-- This module defines the various parser combinators used to parse an HTTP 
-- request and response as a bytestring.  It does *not* define the associated
-- Handler to do so; that is defined in HTTP.  

module HTTP.Parsing 
    (
        message,
        -- * HTTP Component Parsers
        method, target, requestL, statusL, header, http,
        -- ** Utility combinators
        clrf, num, spc,
        -- ** Testing
        testParse,
    )
    where

import HTTP.Types

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C

import qualified Data.Map as Map

import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
  
import Data.ByteString.Builder (toLazyByteString, stringUtf8)

-- | for testing purposes, this function takes a string and
-- parses it via the given parser.  This is not re-exported in HTTP.
testParse :: Parser a -> String -> Result a
testParse p s = parse p $ BS.toStrict $ toLazyByteString $ stringUtf8 s

-- | 
-- Parses a single HTTP request or response in full.
message :: Parser Message
message = do
    _ <- many' clrf
    s <- choice [requestL, statusL]
    h <- manyTill header clrf
    pure $ Message s $ Map.fromList h


-- various combinators building up the above

-- Some basic, small combinators
-- | Parses a carriage return and line feed.
clrf :: Parser ()
clrf = "\r\n" *> pure () <?> "Expected CLRF"

-- | Parses a number.
num :: Parser Int
num = read <$> C.many1 C.digit <?> "Expected a number"

-- | parses a space or tab, as space is expected to be in an HTTP message.
spc :: Parser ()
spc = choice [C.char ' ', C.char '\t'] *> pure () <?> "Expected SP"

-- | Case insensitively parses an HTTP message.
method :: Parser Method
method = choice [
        C.stringCI "GET"     *> pure GET,
        C.stringCI "HEAD"    *> pure HEAD,
        C.stringCI "POST"    *> pure POST,
        C.stringCI "DELETE"  *> pure DELETE,
        C.stringCI "OPTIONS" *> pure OPTIONS,
        C.stringCI "TRACE"   *> pure TRACE] <?> "Invalid Method"

-- | parses an HTTP request-target, a file and options.  
-- This assumes one of 3 forms, as the other two are not for communcating with static servers.  
--
-- The parser will fail to parse if the target file if it notices a "/.." pattern.  
target :: Parser Target
target = do
    -- grab the filename
    file <- C.takeTill (\c -> c == ' ' || c == '?')
    -- and the rest (If it exists)
    optParams <- option "" (C.char '?' *> C.takeTill (== ' '))
    -- decode to text replacing with default error char
    let file' = T.decodeUtf8With T.lenientDecode file
        optParams' = T.decodeUtf8With T.lenientDecode optParams
    -- checks to ensure we have a valid parse
    let validF = T.head file' == '/'
              && (T.null . snd) (T.breakOn "/.." file')
    case validF of
         True -> pure $ Target (T.unpack file') optParams'
         False -> mzero <?> "Invalid file path" -- fail immediately

-- | Parses an HTTP request line.  
-- A request line is one method, the target (/whatever/the/path/is) and the http version.
requestL :: Parser StartLine
requestL = do
    m <- method
    spc                      <?> "Missing SP between method and target"
    t <- target
    spc                      <?> "Missing SP between target and HTTP-version"
    h <- http
    clrf
    pure $ ReqLine m t h

-- | Parses an HTTP status line.  
-- A status line is the http version, the status code, and an ignored optional
statusL :: Parser StartLine
statusL = do
    h <- http
    spc                                        <?> "Missing SP between HTTP-version and status-code"
    c <- read <$> C.count 3 C.digit            <?> "Invalid status code"
    -- reason-phrase (which may not exist)
    -- SHOULD be ignored
    _ <- choice [clrf,
                    spc *>
                    manyTill C.anyChar clrf *>
                    pure ()]
                                               <?> "No SP after status-code or invalid reason-phrase"
    pure $ StatLine h c

-- | Parses a solitary HTTP header.  
header :: Parser Header
header = do
    f <- C.takeWhile1 (/= ':') <?> "Empty field-name"
    _ <- C.char ':' -- takeWhile1 does not clear the ':'
    _ <- many' spc
    v <- BS.pack <$> manyTill anyWord8 clrf <?> "Invalid field-value"
    pure $ (f, v)
    
-- | parses an HTTP version code, such as HTTP/1.1
http :: Parser HTTP
http = do
    _ <- C.stringCI "HTTP" <?> "Expected HTTP"
    _ <- C.char '/'
    major <- num           <?> "Expected major version"
    _ <- C.char '.'
    minor <- num           <?> "Expected minor version"
    pure $ HTTP major minor
