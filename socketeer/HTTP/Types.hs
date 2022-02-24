-- |
-- Module: Types
-- Description: Custom data types to define HTTP messages

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DerivingVia, StandaloneDeriving #-}

module HTTP.Types where

import GHC.Generics
import TextShow 
import TextShow.Generic

import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)

import Data.Map (Map)
import qualified Data.Map as Map

import Sendable

-- | The HTTP version tag.
data HTTP = HTTP Int Int
          deriving (Generic, Show, Eq, Ord)
          deriving TextShow via FromGeneric (HTTP)

-- | All possible http 1.1 methods.  
data Method = 
            GET | HEAD | POST | 
            PUT | DELETE | CONNECT | 
            OPTIONS | TRACE
            deriving (Generic, Show, Ord, Enum, Eq, Bounded)
            deriving TextShow via FromGeneric (Method)

-- | a request-target or Target is simply a path to the requested resource.  
-- While the HTTP spec specifies other forms for this field, they are not intended
-- for communication with things other than servers.  
data Target = Target FilePath Text
            deriving (Generic, Show)
            deriving TextShow via FromGeneric (Target)

-- | A startline is either a request or status line.  
-- Most servers only receive requests, but the datatype contains both to be flexible.  
data StartLine = ReqLine Method Target HTTP
               | StatLine HTTP Int 
               deriving (Show, Generic)
               deriving TextShow via FromGeneric (StartLine)

-- | Header's don't get their own type as we repesent all headers in a message
-- as a map of key-value pairs
type Header = (ByteString, ByteString)

-- | A valid HTTP message consists of a start line and headers.  
--
-- The standard says the message should also contain the body.  
-- However, in the interest of actually responding to the message,
-- the body is left "on the wire."  This does mean that some of it 
-- may be included in the leftover bytes of parsing the message and more will likely 
-- still need to be received.  As such, if the body should be used for anything,
-- it will need to be re-combined.  
--
-- Fortunately, if you are parsing the body (which in almost all cases one would be)
-- you can simply treat the leftovers as the starting ByteString to parse with.
data Message = Message {start :: StartLine, headers :: Map ByteString ByteString}
             deriving (Show, Generic)
             
instance TextShow Message where
    showb m =   fromText ("Message {" :: Text) <> 
                showb (start m) <> 
                showbCommaSpace <> 
                fromString (show . headers $ m) <>
                fromText ("}" :: Text)

-- | Helper function to convert text to bytestrings.
showToBs ::  (TextShow a) => a -> ByteString
showToBs = T.encodeUtf8 . showt

-- | HTTP specs request carriage return and line feed for newlines.
newline :: ByteString
newline = "\r\n"

instance Sendable HTTP where
    packBytes (HTTP maj minor) = "HTTP/" <> showToBs maj <> "." <> showToBs minor

instance Sendable Method where
    packBytes = showToBs

instance Sendable Target where
    packBytes (Target p args) = (T.encodeUtf8 . pack) p <> "?" <> T.encodeUtf8 args

instance Sendable StartLine where
    packBytes (StatLine h code) = packBytes h <> " " <> showToBs code <> newline
    packBytes (ReqLine meth path h) = packBytes meth <> " " <> packBytes path <> " " <> packBytes h <> newline

-- Sendable Message includes sending the Map of headers properly
instance Sendable Message where
    packBytes m = packBytes (start m) <> packMap (headers m) <> newline
        where packMap = Map.foldrWithKey (\name val rest -> name <> ": " <> val <> newline <> rest) ""
