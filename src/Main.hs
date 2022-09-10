module Main where

import           GetUpdates
import           Answer
import           Control.Monad                   ( liftM )
import           Control.Concurrent              ( threadDelay )
import           Data.Time                       ( getZonedTime, zonedTimeToLocalTime, LocalTime )
import           Control.Monad.Trans.Writer.Lazy ( WriterT, runWriterT, tell )



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
  print log
  main_loop $ log_handle new_handle (time,log)