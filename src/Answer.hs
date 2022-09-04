module Answer where

import           GetUpdates
import           Handle
import 			 Config
import           Log
import           Repeats
import           Control.Monad.Trans.Writer.Lazy ( WriterT, runWriterT, tell )
import           Data.ByteString.Lazy.Char8      as Char8      ( ByteString, unpack, empty )
import           Network.HTTP.Simple             ( httpLBS, getResponseBody, parseRequest_ )
import           Control.Monad.IO.Class          ( liftIO )
import           Data.Aeson                      ( eitherDecode )
import           Control.Monad                   ( liftM, replicateM )
import           Data.HashMap.Strict as HM       ( findWithDefault )
import           Data.Aeson                      ( encode )
import           Data.Time
import           Prelude hiding (id)




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
  response_results <- liftIO $ mapM (response_manager $ handle) unaswered_updates
  tell $ if unaswered_updates == [] then [(Info,"No messages to answer")]
                                    else [(Info,"echo message" ++ show response_results)]
  -- write log to handle & return it
  return $ let new_answered_updates = map update_id unaswered_updates
           in write_answered_handle handle local_day new_answered_updates
  


response_manager :: Handle -> UpdateResult -> IO [Char8.ByteString]
response_manager handle upd = do
  x <- answer_commands handle upd
  -- repeat echo message specified number of times
  let answer_echo = let default_ = default_repeats $ config handle
                        user_id  = id . from . message $ upd
                        table    = users_settings $ config handle
                    in  replicateM ( findWithDefault default_ user_id table ) ( echo_message handle upd )
  answer_echo
	
	
	
answer_commands :: Handle -> UpdateResult -> IO [Char8.ByteString]
answer_commands handle upd = do
  let entity_list = entities . message $ upd
  let commands_texts = let commands = filter (\entity -> "bot_command" == (entity_type entity) ) entity_list
                           take_commands_text = ( \entity -> take (entity_length entity) (drop (offset entity ) ( text $ message upd)))
                       in  map take_commands_text commands
  mapM answer_command commands_texts
   where answer_command :: String -> IO Char8.ByteString
         answer_command command = do
             case command of 
                "/help"    -> construct_request handle (chat_id . chat . message $ upd) ( helptext $ config handle)
                "/repeats" -> repeats_request handle (chat_id . chat . message $ upd) --( repeat_text $ config handle)
                others     -> return empty
				
				
repeats_request :: Handle -> Int -> IO Char8.ByteString
repeats_request handle chat_id = do
   let message = repeats_message chat_id (repeat_text $ config handle)
   let json = encode message
   print json
   let request = "https://api.telegram.org/bot" ++ bot_token handle ++
                 "/sendMessage&message="                 ++ show json 
   print request
   res <- httpLBS $ parseRequest_ request
   return (getResponseBody res)



echo_message :: Handle -> UpdateResult -> IO Char8.ByteString
echo_message handle upd = do
  construct_request handle ( chat_id $ chat $ message $ upd ) (text $ message upd)


construct_request :: Handle -> Int -> String -> IO Char8.ByteString
construct_request handle chat_id text = do
  -- constructing & sending echo message request
   let req = "https://api.telegram.org/bot" ++ bot_token handle ++
              "/sendMessage?chat_id="       ++ show chat_id ++ 
              "&text="                      ++ text
   print text
   res <- httpLBS $ parseRequest_ req
   return (getResponseBody res)
  


safe_response :: Char8.ByteString -> WriterT [(Priority,String)] IO Response
safe_response json = do
  let (response,log) = case (eitherDecode json :: Either String Response) of 
                        Right r -> (r,(Info,"Successful parse"))
                        Left s -> (Response { ok = False, result = [] },(Error,"Failed to parse JSON :" ++ s))
  tell [log]
  return response