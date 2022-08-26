module Config where

import           System.Directory
import           System.FilePath
import           Data.Time
import           Control.Monad                   ( liftM )
import           Data.List                       ( groupBy )



get_answered_updates :: IO ()
get_answered_updates = do
  let filename = "answered.txt"
  curr_dir      <- getCurrentDirectory
  cur_dir_cont  <- getDirectoryContents curr_dir
  conf_contents <- if elem filename  cur_dir_cont
                    then readFile $ curr_dir ++ (pathSeparator : filename)
		            else return []
  let raw = words conf_contents
  -- ["date20.08.202298786575","date25.08.2022987675123"]
  let grouped = groupBy ( \x y -> take 10 x == take 10 y ) raw
  -- [["20.08.202298786575"],["25.08.202298786575","25.08.202298786575","25.08.202298786575"]]
  let tupled = map ( map ( \list -> (take 10 list,drop 10 list ) ) ) grouped
  --print tupled
  -- [[("20.08.2022","98786575")],[("25.08.2022","98786575"),("25.08.2022","98786575"),("25.08.2022","98786575")]]
  let cleared = map ( \list -> ( fst $ head list, map (\(date,updateID) -> updateID) list ) ) tupled  
  -- [("20.08.2022",["98786575"]),("25.08.2022",["98786575","98786575","98786575"])]
  let parsed = map ( \(date,list) -> ( toEnum ( read date :: Int) :: Day , map ( \x -> read x :: Int ) list ) ) cleared
  -- [(2022-08-23,[8818140,8818141,8818142,8818143,8818144,8818145,8818146,8818147,8818148])]
  local_day <- liftM ( localDay . zonedTimeToLocalTime ) $ getZonedTime
  let filtered = filter ( \(date,list) -> (fromEnum local_day :: Int) - (fromEnum date :: Int) < 3 ) parsed
  --print (fromEnum local_day :: Int)
  --print (fromEnum ( fst $ head parsed )  :: Int)
  --print filtered
  return ()