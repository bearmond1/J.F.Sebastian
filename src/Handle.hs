module Handle where

import           Data.Time
import           GetUpdates

data Handle = Handle { bot_token :: String,
                       answered_updates :: [Int],
                       update :: Response, 
                       help_text :: String,
                       logger :: Logger }  deriving Show

data Logger = Logger { output_priority :: Priority,
                       log_ :: [( LocalTime,[ (Priority,String) ] )]  }   deriving Show

-- to write comparable instance
data Priority = Debug | Info | Error      deriving Show


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

	  

write_handle_response :: Handle -> Response -> Handle
write_handle_response h r = Handle { bot_token = bot_token h, 
                                     answered_updates = answered_updates h,
                                     update = r,
                                     help_text = help_text h,
                                     logger = logger h  }

					  
log_handle :: Handle -> ( LocalTime,[ (Priority,String) ] ) -> Handle
log_handle h l = Handle { bot_token = bot_token h, 
                          answered_updates = answered_updates h,
                          update = update h,
                          help_text = help_text h,
                          logger = Logger {output_priority = output_priority $ logger h, log_ = [l] ++ (log_ $ logger h)}}


write_answered_handle :: Handle -> [Int] -> Handle
write_answered_handle h l = Handle { bot_token = bot_token h,
                                     answered_updates = l ++ answered_updates h,
                                     update = update h,
                                     help_text = help_text h,
                                     logger = logger h }