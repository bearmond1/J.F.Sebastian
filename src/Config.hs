module Config ( Config ( .. ), get_config, config_filename ) where

import           System.Directory
import           System.FilePath
import           Data.Time
import           Control.Monad                   ( liftM )
import           Data.List                       ( groupBy )
import           Data.HashMap.Strict as HM       ( HashMap, fromList, empty )
import           Data.Time.Calendar.Compat 
import           Log




type User_id = Int
type Repeats = Int


data Config = Config { answered_updates :: HashMap Day [Int],
                       users_settings :: HashMap User_id Repeats,
					   default_repeats :: Int,
					   helptext :: String,
					   repeat_text :: String } deriving (Show, Read)


config_filename :: String
config_filename = "config.txt"

get_config :: IO (Config,Log_entry)
get_config = do
  time         <- liftM zonedTimeToLocalTime $ getZonedTime
  curr_dir     <- getCurrentDirectory
  cur_dir_cont <- getDirectoryContents curr_dir
  contents     <- if elem config_filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : config_filename)
		            else return []
  let (config, log) = if contents == [] 
                       then ( default_config          , (Info,"Empty config file, loaded default settings") )
			           else ( read contents :: Config , (Info,"Config loaded") )
  return (config, (time, [log]) )
  
  
  
default_config :: Config
default_config = Config { answered_updates = empty,
                          users_settings = empty,
						  default_repeats = 1,
						  helptext = default_helptext,
						  repeat_text = default_repeat_text }


default_helptext :: String
default_helptext = "This is simple configurable echo bot. You can bla bla...."

default_repeat_text :: String
default_repeat_text = "Choose number of repaets:"