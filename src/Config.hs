module Config where

import           System.Directory
import           System.FilePath
import           Data.Time
import           Control.Monad                   ( liftM )
import           Data.List                       ( groupBy )



get_answered_updates :: IO ()
get_answered_updates = do
  curr_dir      <- getCurrentDirectory
  cur_dir_cont  <- getDirectoryContents curr_dir
  conf_contents <- if elem "config.txt"  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : "config.txt")
		            else return []
  let raw = words conf_contents
  -- ["date20.08.202298786575","date25.08.2022987675123"]
  let grouped = groupBy ( \x y -> take 10 x == take 10 y ) raw
  -- [["20.08.202298786575"],["25.08.202298786575","25.08.202298786575","25.08.202298786575"]]
  let tupled = map ( map ( \list -> (take 10 list,drop 10 list ) ) ) grouped
  -- [[("20.08.2022","98786575")],[("25.08.2022","98786575"),("25.08.2022","98786575"),("25.08.2022","98786575")]]
  let cleared = map ( \list -> (fst $ head list, map (\(date,updateID) -> read updateID :: Int) list ) ) tupled
  -- [("20.08.2022",[98786575]),("25.08.2022",[98786575,98786575,98786575])]
  print cleared
  local_time <- liftM ( zonedTimeToLocalTime ) $ getZonedTime
  let new_time = diffLocalTime local_time (LocalTime { localDay = toEnum 00000003 , localTimeOfDay = localTimeOfDay local_time } )
  print LocalTime { localDay = toEnum 00000003 , localTimeOfDay = localTimeOfDay local_time }
  print new_time