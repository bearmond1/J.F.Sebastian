module Config ( Config ( .. ), User_id, Repeats, get_config, get_config_ ) where

import           System.Directory
import           System.FilePath
import           Data.Time
import           Control.Monad                   ( liftM )
import           Data.List                       ( groupBy )
import           Data.HashMap.Strict as HM       ( HashMap, fromList )
import           Data.Time.Calendar.Compat 




type User_id = Int
type Repeats = Int


data Config = Config { answered_updates :: HashMap Day [Int],
                       users_settings :: HashMap User_id Repeats,
					   default_repeats :: Int,
					   helptext :: String,
					   repeat_text :: String } deriving (Show, Read)




get_config_ :: IO Config
get_config_ = do
  let filename = "conf.txt"
  curr_dir      <- getCurrentDirectory
  cur_dir_cont  <- getDirectoryContents curr_dir
  contents <- if elem filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : filename)
		            else return []
  return ( read contents )



get_config :: IO Config
get_config = do
  answered_updates <- get_answered_updates
  user_settings    <- get_user_settings
  (default_repeats,helptext,repeat_text)  <- get_common_settings
  
  return $ Config { answered_updates = answered_updates,
                    users_settings = user_settings,
					default_repeats = default_repeats,
					helptext = helptext,
					repeat_text = repeat_text }



get_answered_updates :: IO (HashMap Day [Int])
get_answered_updates = do
  let filename = "answered.txt"
  curr_dir      <- getCurrentDirectory
  cur_dir_cont  <- getDirectoryContents curr_dir
  contents <- if elem filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : filename)
		            else return []
  let list = read contents :: [ (Day,[Int]) ]
  let hashmap = fromList list :: HashMap Day [Int]
  return hashmap
  
  
  
get_user_settings :: IO (HashMap User_id Repeats)
get_user_settings = do
  let filename = "user_settings.txt"
  curr_dir      <- getCurrentDirectory
  cur_dir_cont  <- getDirectoryContents curr_dir
  contents <- if elem filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : filename)
		            else return []
  let list = read contents :: [ (User_id,Repeats) ]
  let hashmap = fromList list :: HashMap User_id Repeats
  return hashmap
  
  
  
get_common_settings :: IO (Int,String,String)
get_common_settings = do
  let filename = "common_settings.txt"
  curr_dir      <- getCurrentDirectory
  cur_dir_cont  <- getDirectoryContents curr_dir
  contents <- if elem filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : filename)
		            else return  "1 This is simple configurable echo bot. You can bla bla.... Choose number of repaets:"
  let settings = words contents
  return $ (read (settings !! 0) :: Int, settings !! 1, settings !! 2 )
  