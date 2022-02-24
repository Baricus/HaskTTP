-- | 
-- Module: Sendable
-- Description: A class to convert custom datatypes to ByteStrings 
--
-- Sendable is largely used to allow for sending HTTP messages simply,
-- providing a way to properly enocde the data types into the proper
-- bytestream for a valid HTTP message.  

module Sendable where

import Data.ByteString (ByteString)

-- | A class to convert arbitrary things to bytes for HTTP
class Sendable a where
    packBytes :: a -> ByteString

-- | A bytestring is (hopefully!) already valid to send over a socket 
-- so we have no reason to further alter it.  
instance Sendable ByteString where
    packBytes = id
