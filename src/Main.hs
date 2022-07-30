{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple            ( httpLBS, getResponseBody, parseRequest_ )
import           Control.Lens                   ( preview )
import           Data.Aeson
import           GHC.Generics
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Data.ByteString.Lazy.Char8 as Char8      ( ByteString, unpack )
import           GetUpdates
import           Control.Monad                  ( when )
import           Control.Concurrent




data Handle = Handle { bot_token :: String,
                       answered_updates :: [Int],
					   update :: Response, 
                       help_text :: String }  deriving Show


getUpdates :: IO Char8.ByteString
getUpdates = do
  res <- httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++  "/getUpdates"
  return (getResponseBody res)


echo_message :: Handle -> IO [Char8.ByteString]
echo_message handle = do
  response_results <- mapM (do_response $ bot_token handle) unaswered_updates
  return response_results
     where unaswered_updates = filter ( \x -> notElem (update_id x) ( answered_updates handle) ) (result $ update handle)


do_response :: String -> UpdateResult -> IO Char8.ByteString
do_response bot_token upd = do
   res <- httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++  
				                    bot_token ++ "/" ++ 
									"sendMessage?chat_id=" ++ 
									show ( chat_id $ chat $ message $ upd ) ++ 
									"&text=" ++ 
									(text $ message upd)
   print "do_response"
   print $ "https://api.telegram.org/bot" ++ bot_token ++ "/" ++ "sendMessage?chat_id=" ++ show ( chat_id $ chat $ message $ upd ) ++ "&text=" ++ (text $ message upd)
   return (getResponseBody res)


safe_response :: Char8.ByteString -> Response
safe_response json = case eitherDecode json :: Either String Response of Right r -> r
                                                                         Left _ -> Response { ok = False, result = [] }


answer_updates :: Handle -> IO Handle
answer_updates handle = do
  res <- httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++ (bot_token handle) ++ "/getUpdates"
  let response = safe_response $ getResponseBody res
  let unaswered_updates = filter ( flip notElem $ answered_updates handle ) $ map ( \x-> update_id x ) $ result response
  echo_message Handle { bot_token = bot_token handle, answered_updates = answered_updates handle, update = response, help_text = "helptext" }
  return Handle { bot_token = bot_token handle, answered_updates = (answered_updates handle) ++ unaswered_updates, update = response, help_text = "helptext" }
  
  
getHandle :: IO Handle
getHandle = do
  json <- getUpdates
  let response = safe_response json
  
  contents <- readFile "C:/Haskell/SimpleHTTP/.stack-work/file.txt"
  let answered_updates = map read $ words contents
  
  token  <- readFile "C:/Haskell/SimpleHTTP/.stack-work/bot_token.txt"
  return Handle { bot_token = token, answered_updates = answered_updates, update = response, help_text = "helptext" }
  
    
closeHandle :: Handle -> IO ()
closeHandle handle = undefined -- ответить на сообщения, записать файлы конфигурации


main_loop :: Handle -> IO ()
main_loop handle = do
 new_handle <- answer_updates handle
 threadDelay 5000000
 main_loop new_handle
 

main :: IO ()
main = do
  handle <- getHandle
  main_loop handle