{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GetUpdates where

import           Data.Aeson   ( FromJSON(..), (.:), (.:?), (.!=), withObject )
import           GHC.Generics ( Generic )

data Response = Response { ok :: Bool,
                           result :: [UpdateResult] }   		deriving ( Eq, Show, Read, Generic )
						   
data UpdateResult = UpdateResult { update_id :: Int,
								   message :: Message }  		deriving ( Eq, Show, Read, Generic )

data Message = Message { message_id :: Int,
						 from :: From,
						 chat :: Chat,
						 date :: Int,
						 text :: String,
					     entities :: [Entity] 
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
					   entity_length :: Int,
					   entity_type :: String }  		deriving ( Eq, Show, Read, Generic )


instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \obj -> do
    chat_id <- obj .: "id"
    chat_first_name <- obj .: "first_name"
    chat_username <- obj .: "username"
    c_type <- obj .: "type"
    return (Chat { chat_id = chat_id, chat_first_name = chat_first_name, chat_username = chat_username, c_type = c_type })
   
   
instance FromJSON Message where  
  parseJSON = withObject "Message" $ \obj -> do
    message_id <- obj .: "message_id"
    from <- obj .: "from"
    chat <- obj .: "chat"
    date <- obj .: "date"
    text <- obj .:? "text" .!= ""
    entities <- obj .:? "entities" .!= []
    return (Message { message_id = message_id, GetUpdates.from = from, chat = chat, date = date, text = text, entities = entities  })
	
	
instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \obj -> do
    offset <- obj .: "offset"
    entity_length <- obj .: "length"
    entity_type <- obj .: "type"
    return (Entity { offset = offset, entity_length = entity_length, entity_type = entity_type })
  

instance FromJSON From
instance FromJSON UpdateResult
instance FromJSON Response
