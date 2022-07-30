{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GetUpdates where

import           Data.Aeson
import           GHC.Generics
import           Data.Text                      ( Text )
import           Control.Applicative
import           Control.Monad

data Response = Response { ok :: Bool,
                           result :: [UpdateResult] }   		deriving ( Eq, Show, Read, Generic )
						   
data UpdateResult = UpdateResult { update_id :: Int,
								   message :: Message }  		deriving ( Eq, Show, Read, Generic )

data Message = Message { message_id :: Int,
						 from :: From,
						 chat :: Chat,
						 date :: Int,
						 text :: String
					     --entities :: [Entity] 
						 }  		deriving ( Eq, Show, Read, Generic )

data From = From { id :: Int,
				   is_bot :: Bool,
				   first_name :: String,
				   username :: String,
				   language_code :: String }  		deriving ( Eq, Show, Read, Generic )

data Chat = Chat { chat_id :: Int,
				   chat_first_name :: String,
				   chat_username :: String,
				   c_type :: String  } deriving ( Eq, Show, Read )
				   
data Entity = Entity { offset :: Int,
					   length :: Int,
					   entity_type :: String }  		deriving ( Eq, Show, Read, Generic )


instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \obj -> do
    chat_id <- obj .: "id"
    chat_first_name <- obj .: "first_name"
    chat_username <- obj .: "username"
    c_type <- obj .: "type"
    return (Chat { chat_id = chat_id, chat_first_name = chat_first_name, chat_username = chat_username, c_type = c_type })
   
   
instance FromJSON From
instance FromJSON Message
instance FromJSON UpdateResult
instance FromJSON Response
instance FromJSON Entity
