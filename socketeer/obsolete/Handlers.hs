module Handlers where

import Network.Socket (Socket, SockAddr) 
import qualified Data.ByteString as BS 
import Network.Socket.ByteString

type Connection = (Socket, SockAddr)
type Handler a b = a -> IO b

-- | ignore the connection
-- Note that the server handles the issue of finalizing this
ignore :: Handler Connection ()
ignore = const $ pure ()

-- | simply reads all bytes sent by the user and then immediately prints them out
echo :: Handler Connection ()
echo p@(sock, _) = do
    val <- recv sock 4096
    sendAll sock val
    if BS.null val then pure () else echo p

