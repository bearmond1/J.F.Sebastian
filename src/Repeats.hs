{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Repeats ( Send_Message(..), repeats_message ) where


import           Data.Aeson ( ToJSON(..), (.=), pairs, Key )
import           GHC.Generics



data Send_Message = Send_Message { to_chat_id :: Int,
                                   send_text  :: String,
								   reply_markup :: Reply_Markup } deriving (Show,Eq,Read,Generic)
								   
data Reply_Markup = InlineKeyboardMarkup | 
                    ReplyKeyboardMarkup_ (ReplyKeyboardMarkup)  |
					ReplyKeyboardRemove  |
					ForceReply 
					   deriving (Show,Eq,Read,Generic)
					
data ReplyKeyboardMarkup = ReplyKeyboardMarkup { keyboard :: [[KeyboardButton]] } deriving (Show,Eq,Read,Generic)

data KeyboardButton = KeyboardButton { keyboard_button_text :: String } deriving (Show,Eq,Read,Generic)


repeats_keyboard :: Reply_Markup
repeats_keyboard = ReplyKeyboardMarkup_ $  ReplyKeyboardMarkup
                   [ [ KeyboardButton { keyboard_button_text = "1" } ],
                     [ KeyboardButton { keyboard_button_text = "2" } ],
					 [ KeyboardButton { keyboard_button_text = "3" } ],
					 [ KeyboardButton { keyboard_button_text = "4" } ],
					 [ KeyboardButton { keyboard_button_text = "5" } ] ]
					 
					 
repeats_message :: Int -> String -> Send_Message
repeats_message chat_id text = Send_Message { to_chat_id = chat_id,
                                              send_text  = text,
								              reply_markup = repeats_keyboard }
					 
					 
					 
instance ToJSON KeyboardButton where 
   toEncoding KeyboardButton{..} = pairs ( "text" .= keyboard_button_text)
	   
	 
instance ToJSON Reply_Markup where
   toEncoding ( ReplyKeyboardMarkup_ (ReplyKeyboardMarkup keyboard_markup) ) = 
       pairs ( "keyboard" .= keyboard_markup )
	   
instance ToJSON ReplyKeyboardMarkup where
   toEncoding ( ReplyKeyboardMarkup keyboard_markup ) = 
       pairs ( "keyboard" .= keyboard_markup)
   

instance ToJSON Send_Message where
   toEncoding Send_Message{..} = 
       pairs ( "chat_id"      .= to_chat_id <>
	           "text"         .= send_text <>
			   "reply_markup" .= reply_markup )
					 