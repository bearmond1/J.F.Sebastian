module Log where


import           Data.Time ( LocalTime )





data Logger = Logger { output_priority :: Priority,
                       log_ :: [Log_entry]  }   deriving Show
					   
type Log_entry = ( LocalTime,[ (Priority,String) ] )



-- to write Eq instance
data Priority = Debug | Info | Error      deriving Show