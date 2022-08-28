module Answer where

import           GetUpdates
import           Handle
import 			 Config
import           Control.Monad.Trans.Writer.Lazy ( WriterT, runWriterT, tell )
import           Data.ByteString.Lazy.Char8      as Char8      ( ByteString, unpack, empty )
import           Network.HTTP.Simple             ( httpLBS, getResponseBody, parseRequest_ )
import           Control.Monad.IO.Class          ( liftIO )
import           Data.Aeson                      ( eitherDecode )
import           Control.Monad                   ( liftM )
import           Data.HashMap.Strict as HM       ( findWithDefault )
import           Data.Time


answer_updates :: Handle -> WriterT [(Priority,String)] IO Handle
answer_updates handle = do
  -- getting updates & time
  res <- liftIO $ httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++ (bot_token handle) ++ "/getUpdates"
  local_day <- liftIO $ liftM ( localDay . zonedTimeToLocalTime ) $ getZonedTime
  -- logging updates recieving, to enhance
  tell $ if null res then [(Handle.Error,"Empty response on getUpdates")]
                     else [(Info,"getUpdates recieved")]
  -- parsing updates
  response <- safe_response $ getResponseBody res
  -- writing accumulated logs, response into handle & answering updates
  let unaswered_updates = filter ( \x -> notElem (update_id x) (findWithDefault [] local_day (answered_updates $ config handle)) ) (result response)
  -- apply echo message to unaswered_updates
  response_results <- liftIO $ mapM (echo_message $ handle) unaswered_updates
  tell $ if unaswered_updates == [] then [(Info,"No messages to answer")]
                                    else [(Info,"echo message" ++ show response_results)]
  -- write log to handle & return it
  return $ write_answered_handle handle local_day $ map update_id unaswered_updates
  
  


echo_message :: Handle -> UpdateResult -> IO Char8.ByteString
echo_message handle upd = do
  -- constructing & sending echo message request
   let req = "https://api.telegram.org/bot" ++  
              bot_token handle ++
              "/sendMessage?chat_id=" ++ 
              show ( chat_id $ chat $ message $ upd ) ++ 
              "&text=" ++ 
              (text $ message upd)
   res <- httpLBS $ parseRequest_ req
   return (getResponseBody res)




safe_response :: Char8.ByteString -> WriterT [(Priority,String)] IO Response
safe_response json = do
  let (response,log) = case (eitherDecode json :: Either String Response) of 
                        Right r -> (r,(Info,"Successful parse"))
                        Left s -> (Response { ok = False, result = [] },(Handle.Error,"Failed to parse JSON :" ++ s))
  tell [log]
  return response