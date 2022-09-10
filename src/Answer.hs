{-# LANGUAGE OverloadedStrings #-}
module Answer (module Handle, module Answer) where

import           Control.Monad.Trans.Writer.Lazy ( WriterT, runWriterT, tell )
import qualified Data.ByteString.Lazy.Char8 as Char8 ( ByteString, empty )
import           Network.HTTP.Simple             ( httpLBS, getResponseBody, parseRequest, setRequestMethod,  setRequestBodyJSON, parseRequest_)
import           Control.Monad.IO.Class          ( liftIO )
import           Data.Aeson                      ( eitherDecode )
import           Control.Monad                   ( liftM, replicateM )
import           Data.HashMap.Strict as HM       ( findWithDefault )
import           Data.Aeson                      ( encode )
import           Data.Time                       ( getZonedTime, ZonedTime(..), localDay )
import           Data.Maybe                      ( catMaybes )
import           Text.Read
import           GetUpdates
import           Handle
import           Repeats


type Is_put_on_hold = Bool


answer_updates :: Handle -> WriterT [(Priority,String)] IO Handle
answer_updates handle = do
  -- getting updates & time
  res <- liftIO $ httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++ (bot_token handle) ++ "/getUpdates"
  local_day <- liftIO $ liftM ( localDay . zonedTimeToLocalTime ) $ getZonedTime
  -- logging updates recieving, to enhance
  tell $ if null res then [(Error,"Empty response on getUpdates")]
                     else [(Info,"getUpdates recieved")]
  -- parsing updates
  response <- safe_response $ getResponseBody res
  -- writing accumulated logs, response into handle & answering updates
  let unaswered_updates = let written_updates = (findWithDefault [] local_day (answered_updates $ config handle))
                              filter_func = ( \x -> notElem (update_id x) written_updates )
                          in  filter filter_func (result response)
  -- apply echo message to unaswered_updates
  new_chats_settings <- liftIO $ mapM (response_manager $ handle) unaswered_updates
  let new_handle = set_chats_on_hold handle $ new_chats_settings
  tell $ if unaswered_updates == [] then [(Info,"No messages to answer")]
                                    else [(Info,"echo message")]
  -- write log to handle & return it
  return $ let new_answered_updates = map update_id unaswered_updates
           in write_answered_handle new_handle local_day new_answered_updates
  


response_manager :: Handle -> UpdateResult -> IO (Chat_id,Repeats)
response_manager handle upd = do
  -- repeat echo message specified number of times
  let chatID   = chat_id . chat . message $ upd
  let usr_settings = get_user_settings handle upd 
					 
  new_chat_repeats <- if usr_settings == 0 
                         then 
                            case (readMaybe ( text . message $ upd ) :: Maybe Int) of
                                (Just x) -> return x
                                Nothing  -> return usr_settings
                         else answer_commands handle upd
  --answer_echo
  print "number_of_repeats"
  print usr_settings
  replicateM usr_settings ( echo_message handle upd )
  return ( chatID, new_chat_repeats )
	
	
	
answer_commands :: Handle -> UpdateResult -> IO Repeats
answer_commands handle upd = do
  --print "answer_commands"
  let entity_list = entities . message $ upd
  let commands_texts = let commands = filter (\entity -> "bot_command" == (entity_type entity) ) entity_list
                           take_commands_text = ( \entity -> take (entity_length entity) (drop (offset entity ) ( text $ message upd)))
                       in  map take_commands_text commands
  is_put_on_hold <- mapM answer_command commands_texts
  return $ if or is_put_on_hold then 0 else get_user_settings handle upd
  
    where answer_command :: String -> IO Is_put_on_hold
          answer_command command = do
             case command of 
                "/help"    -> construct_request handle (chat_id . chat . message $ upd) ( helptext $ config handle)
                "/repeats" -> repeats_request handle (chat_id . chat . message $ upd) 
                others     -> return False
				
				
				
repeats_request :: Handle -> Int -> IO Is_put_on_hold
repeats_request handle chat_id = do
    initialRequest <- parseRequest $ "https://api.telegram.org/bot" ++ bot_token handle ++ "/sendMessage"    
    let send_message = repeats_message chat_id (repeat_text $ config handle)
    let request = setRequestMethod "POST" $ setRequestBodyJSON send_message initialRequest
    response <- httpLBS request
    print (getResponseBody response) 
    return True




echo_message :: Handle -> UpdateResult -> IO Is_put_on_hold
echo_message handle upd = do
  construct_request handle ( chat_id $ chat $ message $ upd ) (text $ message upd)


construct_request :: Handle -> Int -> String -> IO Is_put_on_hold
construct_request handle chat_id text = do
  -- constructing & sending echo message request
   let req = "https://api.telegram.org/bot" ++ bot_token handle ++
              "/sendMessage?chat_id="       ++ show chat_id ++ 
              "&text="                      ++ text
   print req
   res <- httpLBS $ parseRequest_ req
   print res
   return False
  


safe_response :: Char8.ByteString -> WriterT [(Priority,String)] IO GetUpdates.Response
safe_response json = do
  let (response,log) = case (eitherDecode json :: Either String GetUpdates.Response) of 
                        Right r -> (r,(Info,"Successful parse"))
                        Left s -> (Response { ok = False, result = [] },(Error,"Failed to parse JSON :" ++ s))
  tell [log]
  return response