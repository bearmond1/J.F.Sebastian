{-# LANGUAGE OverloadedStrings #-}

module Main where
 
import           Network.HTTP.Simple             ( httpLBS, getResponseBody, parseRequest_ )
import           GetUpdates
import           Handle
import           Answer
import           Config
import           Control.Monad                   ( liftM )
import           Control.Concurrent              ( threadDelay )
import           Data.Time
import           Control.Monad.Trans.Writer.Lazy ( WriterT, runWriterT, tell )
import           Control.Monad.IO.Class          ( liftIO )



main :: IO ()
main = do
  -- initialize hanlde & start program cycle
  handle <- getHandle
  main_loop handle


main_loop :: Handle -> IO ()
main_loop handle = do
  time <- liftM zonedTimeToLocalTime $ getZonedTime
  (new_handle,log) <- runWriterT $ answer_updates handle
  closeHandle new_handle
  threadDelay 2000000
  main_loop $ log_handle new_handle (time,log)