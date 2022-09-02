{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Repeats where


import           Data.Aeson ( toEncoding )
import           GHC.Generics



data Send_Message = Send_Message { to_chat_id :: Int,
                                   send_text  :: String,
								   reply_markup :: Reply_Markup }
								   
data Reply_Markup = InlineKeyboardMarkup | 
                    ReplyKeyboardMarkup_ (ReplyKeyboardMarkup)  |
					ReplyKeyboardRemove  |
					ForceReply
					
data ReplyKeyboardMarkup = ReplyKeyboardMarkup { keyboard :: [[KeyboardButton]] } deriving (Show,Eq,Read,Generic)

data KeyboardButton = KeyboardButton { keyboard_button_text :: String }


repeats_keyboard :: Reply_Markup
repeats_keyboard = ReplyKeyboardMarkup 
                   [ [ KeyboardButton { keyboard_button_text = "1" } ]
                     [ KeyboardButton { keyboard_button_text = "2" } ]
					 [ KeyboardButton { keyboard_button_text = "3" } ]
					 [ KeyboardButton { keyboard_button_text = "4" } ]
					 [ KeyboardButton { keyboard_button_text = "5" } ] ]
					 
					 
					 
instance toJSON KeyboardButton where 
   toEncoding KeyboardButton{..} = 
       pairs ("text" .= keyboard_button_text)
	   
	 
instance toJSON Reply_Markup

instance toJSON Send_Message where
   toJSON Send_Message{..} = object [
     "chat_id"      .= to_chat_id
	 "text"         .= send_text
	 "reply_markup" .= reply_markup ]
					 