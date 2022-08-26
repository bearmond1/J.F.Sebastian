module Handle where

import           Data.Time
import           GetUpdates
import           Control.Monad              ( when, liftM )
import           Data.Time
import           Data.HashMap.Strict as HM  ( HashMap, update, empty, insert, lookup )

data Handle = Handle { bot_token :: String,
                       answered_updates :: HashMap Day [Int],
                       update_response :: Response, 
                       help_text :: String,
                       logger :: Logger }  deriving Show
					   
--type Answered_Updates = HashMap Day [Int]

data Logger = Logger { output_priority :: Priority,
                       log_ :: [( LocalTime,[ (Priority,String) ] )]  }   deriving Show

-- to write comparable instance
data Priority = Debug | Info | Error      deriving Show


-- Initialize
getHandle :: IO Handle
getHandle = do  
  contents <- readFile "C:/Haskell/SimpleHTTP/.stack-work/file.txt"
  let answered_updates = empty --map read $ words contents
  
  token <- readFile "C:/Haskell/SimpleHTTP/.stack-work/bot_token.txt"
  return Handle { bot_token = token, 
                  answered_updates = empty :: HashMap Day [Int], 
                  update_response = Response { ok = False, result = [] }, 
                  help_text = "helptext",
                  logger = Logger { output_priority = Info, log_ = [] } }


-- Exit
closeHandle :: Handle -> IO ()
closeHandle handle = do
  -- ответить на сообщения, записать файлы конфигурации  
  local_day <- liftM ( localDay . zonedTimeToLocalTime ) $ getZonedTime
  let content = "" --unwords $ map ( \int -> show_day local_day ++ show int) $ answered_updates handle
  when (length content > 0) $ writeFile "answered.txt" $ content
    where show_day :: Day -> String
          show_day d = show (fromEnum d :: Int)

	  

write_handle_response :: Handle -> Response -> Handle
write_handle_response h r = Handle { bot_token = bot_token h, 
                                     answered_updates = answered_updates h,
                                     update_response = r,
                                     help_text = help_text h,
                                     logger = logger h  }


log_handle :: Handle -> ( LocalTime,[ (Priority,String) ] ) -> Handle
log_handle h l = Handle { bot_token = bot_token h, 
                          answered_updates = answered_updates h,
                          update_response = update_response h,
                          help_text = help_text h,
                          logger = Logger {output_priority = output_priority $ logger h, log_ = [l] ++ (log_ $ logger h)}}


write_answered_handle :: Handle -> Day -> [Int] -> Handle
write_answered_handle handle day list = 
  let old_upd = ( answered_updates handle )
      new_log = case HM.lookup day old_upd of
		         (Just x) -> update ( \x -> Just (list ++ x) ) day ( answered_updates handle )
		         Nothing  -> insert day list ( answered_updates handle )
  in append_answered handle new_log
  

append_answered :: Handle -> HashMap Day [Int] -> Handle
append_answered handle list = Handle { bot_token = bot_token handle,
                                       answered_updates = list,
                                       update_response = update_response handle,
                                       help_text = help_text handle,
                                       logger = logger handle }