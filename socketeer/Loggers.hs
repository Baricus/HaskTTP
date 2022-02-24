{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
-- | 
-- Module: Loggers
-- Description: Functions that output a log to an output device in IO
--
-- A Logger is a function that takes in
-- a log representation and logs it to
-- an output source, whether that's standard
-- out, a file, or somewhere else
--
-- Note that most functions in this file expect
-- a string list of separators which is used to
-- construct a new logger.  This is to enable
-- nicer formatting while still keeping things generic
module Loggers 
    (
        -- * Data Types
        Logger (..), Sep, Seps,
        -- * Loggers
        dropLog,
        -- ** Generic Logging
        -- $generics
        logOut, logOut', logOutP,
        -- ** StdOut Logging
        toStdOut, toStdOut', toStdOutP,
        -- ** File Logging
        toFile, toFile', toFileP,
        -- * Utility
        intercalateM, 
        -- $combinators
        listComb, singleSep,
    )
    where

import Data.Text (Text)
import qualified Data.Text.IO as T

import Handlers (Log)

-- | A separator is used to split logged values.  
type Sep = Text
-- | Most logger functions take lists of separtors.  
-- This is to allow for different separators between each value in the log.  
type Seps = [Sep]

-- | Loggers are composable functions from the log result of a handler
-- to IO units.  They abstract writing the given log to X device.  
--
-- Composing them as semigroupoids or monoids simply returns a new Logger that logs
-- to multiple devices in potentially varying formats.  
newtype Logger = Logger {writeLog :: Log -> IO ()}
    deriving newtype (Semigroup, Monoid)

-- | intercalateM
-- takes a default separator, a list of separators (in order) and the final list
-- to return a single value combining them.  
--
-- This is used in loggers to combine the given log and separators.  
intercalateM :: Monoid a => a -> [a] -> [a] -> a
intercalateM _ _ [] = mempty
intercalateM def seps (v:vs) = mconcat $ v : zipWith (\s v' -> s <> v') (seps <> repeat def) vs

-- $combinators
-- The two main methods we have for formatting logs are:
-- 
--     1. A user supplied list of separators with a fallback value
--     2. A user supplied (single) separator which should always be used
--          
-- The following functions define these methods in order.  
-- $combinators

-- | listComb combines a list of separators with a log in an alternating pattern, starting with the log, i.e.:
--
-- >>> intercalateM ["1", "2", "3"] ["L", "O", "G"]
-- L1O2G
--
-- Note that not all separators need to be used; any excess will be ignored.  
-- Also, if there are not enough separators, this function uses spaces by default.  
listComb :: Seps -> Log -> Text
listComb s l = intercalateM " " s l

-- | singleSep supplies only one separator and uses it between every log value.  
singleSep :: Sep -> Log -> Text
singleSep s l = intercalateM s [] l

-- $generics
-- Generic "Log Text to some output" functions are used to generate all other
-- log functions by supplying them with a @Text -> IO ()@ function, a "logging method" to output
-- the log to something, whether that's a file, stdout, or something else entirely.
-- $generics

-- | dropLog simply discards the entire log explicitly, in case logging is not desired.  
dropLog :: Logger
dropLog = Logger $ const $ pure ()

-- | logOut takes in a logging method and a list of separators and produces a new logger
-- which will format by the given specifiers and use the given output method.
logOut :: (Text -> IO ()) -> [Text] -> Logger
logOut f seps = Logger $ (\l -> f $ listComb seps l <> "\n")

-- | logOut'
-- Functions similarly to logOut' but only asks for one separator, which is
-- used to separate all entries.
logOut' :: (Text -> IO ()) -> Sep -> Logger
logOut' f sep = logOut f $ repeat sep

-- | logOutP(lain) also functions similarly to logOut, but simply inserts a single space between all values.
logOutP :: (Text -> IO ()) -> Logger
logOutP f = logOut f []

-- Specific instantiations of the above general functions

-- Standard Output Logging
-- | 
-- Logs to standard out, supplied with separators.
toStdOut :: Seps -> Logger
toStdOut = logOut T.putStr

-- | 
-- Logs to standard out, supplied with a single separator to repeat.
toStdOut' :: Sep -> Logger
toStdOut' = logOut' T.putStr

-- | 
-- Logs to standard output, putting a space between each value.
toStdOutP :: Logger
toStdOutP = toStdOut []

-- | Logs to a given file, supplied with separators.  
toFile :: FilePath -> Seps -> Logger
toFile = logOut . T.appendFile 

-- | Logs to a given file, supplied with a single separator to repeat.  
toFile' :: FilePath -> Sep -> Logger
toFile' = logOut' . T.appendFile

-- | Logs to a given file, putting a space between each value.
toFileP :: FilePath -> Logger
toFileP = logOutP . T.appendFile
