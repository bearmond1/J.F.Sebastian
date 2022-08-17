{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple            ( httpLBS, getResponseBody, parseRequest_ )
import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text.IO                   as TIO
import           Data.ByteString.Lazy.Char8     as Char8      ( ByteString, unpack, empty )
import           GetUpdates
import           Control.Monad                  ( when, liftM )
import           Control.Concurrent             ( threadDelay )
--import           Control.Monad.Trans
import           Data.Time                    



data Handle = Handle { bot_token :: String,
                       answered_updates :: [Int],
                       update :: Response, 
                       help_text :: String,
                       logger :: Logger }  deriving Show

data Logger = Logger { output_priority :: Priority,
                       log_ :: [[(Priority,LocalTime,String)]]  }   deriving Show

data Priority = Debug | Info | Error      deriving Show



answer_updates :: Handle -> IO Handle
answer_updates handle = do
  res <- httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++ (bot_token handle) ++ "/getUpdates"
  time <- liftM zonedTimeToLocalTime $ getZonedTime
  let upd_log = if null res then (Main.Error,time,"Empty response on getUpdates") 
                            else (Info,time,"getUpdates recieved")
  (response,parse_log) <- safe_response $ getResponseBody res  
  answer_handle $ write_handle_response ( log_handle_next handle [parse_log,upd_log] ) response
  


answer_handle :: Handle -> IO Handle
answer_handle handle = do
  time <- liftM zonedTimeToLocalTime $ getZonedTime
  let unaswered_updates = filter ( \x -> notElem (update_id x) (answered_updates handle) ) (result $ update handle)
  response_results <- mapM (echo_message $ handle) unaswered_updates
  let response_log = if unaswered_updates == [] then [(Info,time,"No messages to answer")]
                                                 else [(Info,time,"echo message" ++ show response_results)]
  return $ write_answered_handle ( log_handle handle response_log ) $ map update_id unaswered_updates


echo_message :: Handle -> UpdateResult -> IO Char8.ByteString
echo_message handle upd = do
   let req = "https://api.telegram.org/bot" ++  
              bot_token handle ++
              "/sendMessage?chat_id=" ++ 
              show ( chat_id $ chat $ message $ upd ) ++ 
              "&text=" ++ 
              (text $ message upd)
   res <- httpLBS $ parseRequest_ req
   return (getResponseBody res)



main :: IO ()
main = do
  handle <- getHandle
  main_loop handle
  
  
main_loop :: Handle -> IO ()
main_loop handle = do
 new_handle <- answer_updates handle
 print $ logger new_handle
 threadDelay 2000000
 main_loop new_handle
 


  
-- Initialize
getHandle :: IO Handle
getHandle = do  
  contents <- readFile "C:/Haskell/SimpleHTTP/.stack-work/file.txt"
  let answered_updates = map read $ words contents
  
  token <- readFile "C:/Haskell/SimpleHTTP/.stack-work/bot_token.txt"
  return Handle { bot_token = token, 
                  answered_updates = answered_updates, 
                  update = Response { ok = False, result = [] }, 
                  help_text = "helptext",
                  logger = Logger { output_priority = Info, log_ = [[]] } }
  

-- Exit
closeHandle :: Handle -> IO ()
closeHandle handle = undefined -- ответить на сообщения, записать файлы конфигурации


safe_response :: Char8.ByteString -> IO (Response,(Priority,LocalTime,String))
safe_response json = do
  time <- liftM zonedTimeToLocalTime $ getZonedTime
  case eitherDecode json :: Either String Response 
   of Right r -> return (r,(Info,time,"Successful parse"))
      Left s -> return (Response { ok = False, result = [] },(Main.Error,time,"Failed to parse JSON :" ++ s))

	  

write_handle_response :: Handle -> Response -> Handle
write_handle_response h r = Handle { bot_token = bot_token h, 
                                     answered_updates = answered_updates h,
                                     update = r,
                                     help_text = help_text h,
                                     logger = logger h  }

log_handle :: Handle -> [(Priority,LocalTime,String)] -> Handle
log_handle h l = Handle { bot_token = bot_token h, 
                          answered_updates = answered_updates h,
                          update = update h,
                          help_text = help_text h,
                          logger = Logger { output_priority = output_priority $ logger h, log_ = log  } }
				where log = if length (log_ $ logger h) == 0 then [l] else [l ++ head (log_ $ logger h)] ++ tail (log_ $ logger h)
				
log_handle_next :: Handle -> [(Priority,LocalTime,String)] -> Handle
log_handle_next h l = Handle { bot_token = bot_token h, 
                               answered_updates = answered_updates h,
                               update = update h,
                               help_text = help_text h,
                               logger = Logger { output_priority = output_priority $ logger h, log_ = [l] ++ (log_ $ logger h) } }
				--where log = if length (log_ $ logger h) == 0 then [l] else [l ++ head (log_ $ logger h)] ++ tail (log_ $ logger h)

write_answered_handle :: Handle -> [Int] -> Handle
write_answered_handle h l = Handle { bot_token = bot_token h,
                                     answered_updates = l ++ answered_updates h,
                                     update = update h,
                                     help_text = help_text h,
                                     logger = logger h }

