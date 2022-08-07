{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple            ( httpLBS, getResponseBody, parseRequest_ )
import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Data.ByteString.Lazy.Char8 as Char8      ( ByteString, unpack, empty )
import           GetUpdates
import           Control.Monad                  ( when )
import           Control.Concurrent
import           Control.Monad.Trans



data Handle = Handle { bot_token :: String,
                       answered_updates :: [Int],
                       update :: Response, 
                       help_text :: String,
                       logger :: Logger }  deriving Show
					   
data Logger = Logger { output_priority :: Priority,
                       log_ :: [(Priority,String)]  }   deriving Show
					   
data Priority = Debug | Info | Error      deriving Show


answer_updates :: Handle -> IO Handle
answer_updates handle = do
  res <- httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++ (bot_token handle) ++ "/getUpdates"
  let new_log = log_answer res
  let response = safe_response $ getResponseBody res
  let unaswered_updates = filter ( flip notElem $ answered_updates handle ) $ map ( \x-> update_id x ) $ result response
  answer_handle Handle { bot_token = bot_token handle, 
                         answered_updates = answered_updates handle, 
                         update = response, 
                         help_text = "helptext",
                         logger = Logger { output_priority = Info, log_ = [] }  }
				  
  return Handle { bot_token = bot_token handle, 
                  answered_updates = (answered_updates handle) ++ unaswered_updates, 
                  update = response, 
                  help_text = "helptext",
                  logger = Logger { output_priority = Info, log_ = [] }  }
				  
    where log_answer res = if null res then (Main.Error,"Empty response on :" ++ (show res)) : ( log_ $ logger handle )
                            else (Info,"getUpdates response : " ++ show res) : ( log_ $ logger handle )


answer_handle :: Handle -> IO [Char8.ByteString]
answer_handle handle = do
  response_results <- mapM (echo_message $ bot_token handle) unaswered_updates
  return response_results
     where unaswered_updates = filter ( \x -> notElem (update_id x) ( answered_updates handle) ) (result $ update handle)


echo_message :: String -> UpdateResult -> IO Char8.ByteString
echo_message bot_token upd = do
   let req = "https://api.telegram.org/bot" ++  
              bot_token ++ "/" ++ 
              "sendMessage?chat_id=" ++ 
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
                  logger = Logger { output_priority = Info, log_ = [] } }
  

-- Exit
closeHandle :: Handle -> IO ()
closeHandle handle = undefined -- ответить на сообщения, записать файлы конфигурации


safe_response :: Char8.ByteString -> Response
safe_response json = case eitherDecode json :: Either String Response of Right r -> r
                                                                         Left _ -> Response { ok = False, result = [] }