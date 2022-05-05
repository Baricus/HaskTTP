{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.FilePath

import Data.Bool (bool)
import Data.ByteString (ByteString, pack)

import Handlers
import HTTP

import qualified Data.Map as M

import System.Directory
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Network.Mime

-- | constructIndex
-- takes a filepath and returns an index.html file of all files/folders in that directory
-- as links that are clickable
--
-- This assumes it is a directory and will throw an error if given something contrary
--
-- The HTML for a directory is based off the python http.server module's HTTP, with minor modifications
constructIndex :: FilePath -> IO (ByteString)
constructIndex folderPath = do
    folderName <- makeRelativeToCurrentDirectory folderPath
    -- ensure we don't have "Directory Listing for /./"
    let folder | folderName == "." = "/"
               | otherwise = "/" <> (T.encodeUtf8 . T.pack) folderName <> "/"
    let title = "Directory listing for " <> folder
        start = "<!DOCTYPE HTML>"
             <> "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
             <> "<title>" 
             <> title
             <> "</title></head><body><h1>" <> title <> "</h1><hr><ul>"
        end = "</ul><hr><footer><p><a href=\"/\">Return to /</a></p><p>" <> pack [0xF0, 0x9D, 0x9B, 0x8C] <> "</p></footer></body></html>"
    -- convert directory listing to relative paths (ignoring anything we can't open)
    files <- listDirectory folderPath
    -- add trailing slashes to any folders
    files' <- mapM (\f -> doesDirectoryExist (folderPath </> f) >>= bool (pure f) (pure $ f ++ "/")) files 
    -- use those paths to build the links ans assemble the page
    pure $ start <> foldr (\file rest -> let 
                              urlPath = T.encodeUtf8 $ T.pack file
                        -- one bulleted link in the file
                    in "<li><a href=\"" <> urlPath <> "\">" <> urlPath <> "</a></li>" <> rest) end files'


-- | getHeaders
-- Returns the headers for a request given either the mime type or the filename.
-- Currently this just creates the Content-Type header.
getHeaders :: Either MimeType FilePath -> M.Map ByteString ByteString
getHeaders input = let mtype = case input of 
                                Left t     -> t
                                Right file -> defaultMimeLookup . T.pack $ takeFileName file
                    in M.fromList [("Content-Type", mtype)]

-- | handleGet
-- serves content from get requests
-- Does not care about anything else on the wire or leftover
handleGet :: (Message, ByteString) -> Handler ()
handleGet (Message{start = (ReqLine GET (Target resource _) _)}, _) = do
    -- the path needs to start with a / but it's really relative
    when (head resource /= '/') $ send400L M.empty -- checked in parser but re-confirmed here
    path <- liftIO $ makeAbsolute (drop 1 resource) >>= canonicalizePath
    -- if we can't find it as a file or directory, 404 file not found
    exists <- liftIO $ doesPathExist path
    if (not exists) 
       then (sendStatL 404 M.empty) 
       else handleExisting path
    where handleExisting path = do
              -- if it's a file (not a directory) send it, else send our main page
              isFile <- liftIO $ doesFileExist path
              if isFile then handleFile path else handleDir path
          -- to handle a file, just send the content TODO MIME types
          handleFile path = send200L (getHeaders $ Right path) *> sendFile path
          -- to handle a directory, build the new index page if it doesn't already exist
          handleDir dir  = do
              -- see if an index.html exists in this directory to send
              let file = (dir <> "/index.html")
              indexExists <- liftIO $ doesFileExist file
              if indexExists 
                 -- send the file directly
                 then send200L (getHeaders $ Right file) *> sendFile file
                 -- create our index and send it
                 else send200L (getHeaders $ Left "text/html") *> (liftIO $ constructIndex dir) >>= sendAll
-- anything else means ignore
handleGet _ = pure ()

-- | handleHead
-- does the exact same thing as a GET request, but does not actually send the file;
-- does not include content-length as it is not required
handleHead :: (Message, ByteString) -> Handler ()
handleHead (Message{start = (ReqLine HEAD (Target resource _) _)}, _) = do
    when (head resource /= '/') $ send400L M.empty -- checked in parser but re-confirmed here
    path <- liftIO $ makeAbsolute (drop 1 resource) >>= canonicalizePath
    -- if we can't find it as a file or directory, 404 file not found
    exists <- liftIO $ doesPathExist path
    if (not exists) then send404L M.empty else send200L (getHeaders $ Right path)
handleHead _ = pure ()

-- | handleUnimplemented
-- returns a 501 request for anything but a GET and HEAD request since that's all we've written
handleUnimplemented :: (Message, ByteString) -> Handler ()
handleUnimplemented (Message{start = (ReqLine a _ _)}, _) = case a of
                                                            GET -> pure ()
                                                            HEAD -> pure ()
                                                            _ -> sendStatL 501 M.empty
-- status line sent to server means bad request, but we'll have fun here
handleUnimplemented _ = sendStatL 418 M.empty 
