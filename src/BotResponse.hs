
-- Data structure of bot responses

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module BotResponse where

import Data.Aeson
import GHC.Generics

newtype Update = Update {
    updateContent :: UpdateContent
} deriving (Generic, Show)

instance FromJSON Update where
    parseJSON = genericParseJSON defaultOptions {unwrapUnaryRecords = True}

data UpdateContent = MessageUpdateContent MessageUpdate | InlineQueryUpdateContent InlineQueryUpdate
    deriving (Generic, Show)

instance FromJSON UpdateContent where
    parseJSON = genericParseJSON defaultOptions {sumEncoding = UntaggedValue}

data MessageUpdate = MessageUpdate {
    update_id :: Integer,
    message :: Message
} deriving (Generic, FromJSON, Show)

newtype InlineQueryUpdate = InlineQueryUpdate {
    inline_query :: InlineQuery
} deriving (Generic, ToJSON, FromJSON, Show)

data InlineQuery = InlineQuery {
    -- id :: String,
    query :: String
} deriving (Generic, ToJSON, FromJSON, Show)

data Message = Message {
    message_id :: Integer,
    text :: String,
    chat :: Chat
} deriving (Generic, ToJSON, FromJSON, Show)

newtype Chat = Chat {
    id :: Integer
} deriving (Generic, ToJSON, FromJSON, Show)

data Response = Response {
    ok :: Bool,
    result :: [Update]
} deriving (Generic, FromJSON, Show)