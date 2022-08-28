module Handle where

import           Data.Time
import           GetUpdates
import           System.Directory
import           System.FilePath
import           Config
import           Control.Monad              ( when, liftM )
import           Data.Time
import           Data.HashMap.Strict as HM  ( HashMap, update, empty, insert, lookup )

data Handle = Handle { bot_token :: String,
                       config :: Config,
                       update_response :: Response, 
                       logger :: Logger }  deriving Show


data Logger = Logger { output_priority :: Priority,
                       log_ :: [( LocalTime,[ (Priority,String) ] )]  }   deriving Show



-- to write Eq instance
data Priority = Debug | Info | Error      deriving Show


-- Initialize
getHandle :: IO Handle
getHandle = do
  config <- get_config
  
  let filename = "bot_token.txt"
  curr_dir      <- getCurrentDirectory
  cur_dir_cont  <- getDirectoryContents curr_dir
  token_contents <- if elem filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : filename)
		            else  return $ error ("Unable to find bot_token.txt in " ++ 
					           curr_dir ++ (pathSeparator : filename) ++ 
							   ". Bot cant work without token)" )

  return Handle { bot_token = read token_contents, 
                  config = config, 
                  update_response = Response { ok = False, result = [] },
                  logger = Logger { output_priority = Info, log_ = [] } }


-- Exit
closeHandle :: Handle -> IO ()
closeHandle handle = do
  -- ответить на сообщения, записать файлы конфигурации
  let answered = show $ config handle
  when (length answered > 0) $ writeFile "conf.txt" $ answered

	  

write_handle_response :: Handle -> Response -> Handle
write_handle_response h r = Handle { bot_token = bot_token h, 
                                     config = config h,
                                     update_response = r,
                                     logger = logger h  }


log_handle :: Handle -> ( LocalTime,[ (Priority,String) ] ) -> Handle
log_handle h l = Handle { bot_token = bot_token h, 
                          config = config h,
                          update_response = update_response h,
                          logger = Logger {output_priority = output_priority $ logger h, log_ = [l] ++ (log_ $ logger h)}}


write_answered_handle :: Handle -> Day -> [Int] -> Handle
write_answered_handle handle day list = 
  let old_upd = ( answered_updates $ config handle )
      new_log = case HM.lookup day old_upd of
		         (Just x) -> update ( \x -> Just (list ++ x) ) day ( answered_updates $ config handle )
		         Nothing  -> insert day list ( answered_updates $ config handle )
  in append_answered handle new_log
  

append_answered :: Handle -> HashMap Day [Int] -> Handle
append_answered handle list = 
  let old_config = config handle in
  Handle { bot_token = bot_token handle,
           config = Config { answered_updates = answered_updates old_config,
                             users_settings = users_settings old_config,
							 default_repeats = default_repeats old_config,
							 helptext = helptext old_config,
							 repeat_text = repeat_text old_config },
		   update_response = update_response handle,
           logger = logger handle }