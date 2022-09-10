module Handle ( module Config, module Handle ) where

import           Data.Time                  ( getZonedTime, zonedTimeToLocalTime, LocalTime, Day )
import           System.Directory           ( getCurrentDirectory, getDirectoryContents )
import           System.FilePath            ( pathSeparator )
import           Control.Monad              ( when, liftM )
import           Data.HashMap.Strict as HM  ( HashMap, update, empty, insert, lookup, findWithDefault )
import           GetUpdates
import           Config

data Handle = Handle { bot_token :: String,
                       config :: Config,
                       update_response :: Response, 
                       logger :: Logger }  deriving Show

type Answered_upd_id = Int


-- Initialize
getHandle :: IO Handle
getHandle = do
  (config, conf_log) <- get_config
  --print config
  let filename = "bot_token.txt"
  curr_dir       <- getCurrentDirectory
  cur_dir_cont   <- getDirectoryContents curr_dir
  token_contents <- if elem filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : filename)
		            else  return $ error ("Unable to find bot_token.txt in " ++ 
					           curr_dir ++ (pathSeparator : filename) ++ 
							   ". Bot cant work without token)" )
  --print token_contents
  return Handle { bot_token = token_contents, 
                  config = config, 
                  update_response = Response { ok = False, result = [] },
                  logger = Logger { output_priority = Info, log_ = [conf_log] } }


-- Exit
closeHandle :: Handle -> IO ()
closeHandle handle = do
  -- ответить на сообщения, записать файлы конфигурации
  let conf_content = show $ config handle
  when (length conf_content > 0) $ writeFile config_filename $ conf_content

	  

write_handle_response :: Handle -> Response -> Handle
write_handle_response h r = Handle { bot_token = bot_token h, 
                                     config = config h,
                                     update_response = r,
                                     logger = logger h  }


log_handle :: Handle -> Log_entry -> Handle
log_handle h l = Handle { bot_token = bot_token h, 
                          config = config h,
                          update_response = update_response h,
                          logger = Logger {output_priority = output_priority $ logger h, 
						                   log_ = [l] ++ (log_ $ logger h) } 
						}


write_answered_handle :: Handle -> Day -> [Answered_upd_id] -> Handle
write_answered_handle handle day list = 
  let old_upd = ( answered_updates $ config handle )
      new_upd = case HM.lookup day old_upd of
		         (Just x) -> update ( \x -> Just (list ++ x) ) day old_upd
		         Nothing  -> insert day list old_upd
  in append_answered handle new_upd
  

append_answered :: Handle -> HashMap Day [Answered_upd_id] -> Handle
append_answered handle list = 
  let old_config = config handle in
  Handle { bot_token = bot_token handle,
           config = Config { answered_updates = list,
                             users_settings = users_settings old_config,
							 default_repeats = default_repeats old_config,
							 helptext = helptext old_config,
							 repeat_text = repeat_text old_config },
		   update_response = update_response handle,
           logger = logger handle }




set_chats_on_hold :: Handle -> [(Chat_id,Repeats)] -> Handle
set_chats_on_hold handle [] = handle
set_chats_on_hold handle [(chat,repeats)] = 
   let new_settings = insert chat repeats $ (users_settings $ config handle)
   in update_users_settings handle new_settings
set_chats_on_hold handle ((chat,repeats):rest) = 
   let new_settings = insert chat repeats $ ( users_settings . config $ set_chats_on_hold handle rest)
   in update_users_settings handle new_settings
   
   
   
update_users_settings :: Handle -> HashMap Chat_id Repeats -> Handle
update_users_settings h users_settings =
   let old_config = config h in 
   Handle { bot_token = bot_token h, 
            config = Config { answered_updates = answered_updates old_config,
                              users_settings = users_settings,
							  default_repeats = default_repeats old_config,
							  helptext = helptext old_config,
							  repeat_text = repeat_text old_config } ,
            update_response = update_response h,
            logger = logger h  }
			
			
get_user_settings :: Handle -> UpdateResult -> Int
get_user_settings handle upd = 
   let default_ = default_repeats $ config handle
       usrs_settings = users_settings . config $ handle
       chatID = chat_id . chat . message $ upd
   in  findWithDefault default_ chatID usrs_settings