{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Lib
    ( requestBot
    ) where

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import Data.List (intercalate)
import Control.Monad (mapM_)
import Data.Text.Array (copyM)
import Config (tokenPath)
import BotResponse


getBotToken :: IO String
getBotToken = readFile tokenPath

getRequestURL :: String -> IO String
getRequestURL method = do
    token <- getBotToken
    return $ "https://api.telegram.org/bot" ++ token ++ "/" ++ method

fetchJSON :: Integer -> IO BS.ByteString
fetchJSON offset = do
    let timeout = 60 :: Int
    let params = getParamsStr [
                ("offset", show offset),
                ("timeout", show timeout)
            ]
    requestURL <- getRequestURL $ "getUpdates?" ++ params
    request <- parseRequest requestURL
    res <- httpBS request
    return $ getResponseBody res

-- Sends duplicate of a received message to the chatroom
copyMessage :: Update -> IO ()
copyMessage Update {updateContent = (MessageUpdateContent MessageUpdate {message = Message {message_id = msgId, chat = Chat chatId}})} = do
    let params = [
                ("chat_id", show chatId),
                ("from_chat_id", show chatId),
                ("message_id", show msgId)
            ]
    requestURL <- getRequestURL $ "copyMessage?" ++ getParamsStr params
    request <- parseRequest requestURL
    httpBS request
    return ()
copyMessage Update {updateContent = _} = error ""

sendHelpMessage :: IO ()
sendHelpMessage = putStrLn "this is help"

processUpdate :: Update -> IO ()
processUpdate update@Update {updateContent = (MessageUpdateContent MessageUpdate {message = Message {text = text}})}
    | text == "/help" = sendHelpMessage
    | text == "/repeat" = print "repeat"
    | otherwise = copyMessage update
processUpdate Update {updateContent = _} = error ""

getParamsStr :: [(String, String)] -> String
getParamsStr a = intercalate "&" $ (\(key, val) -> key ++ "=" ++ val) <$> a

getUpdateId :: Update -> Integer
getUpdateId Update {updateContent = (MessageUpdateContent MessageUpdate {update_id = updateId})} = updateId
getUpdateId Update {updateContent = _} = error ""

isMessageUpdate :: Update -> Bool
isMessageUpdate (Update (MessageUpdateContent _)) = True
isMessageUpdate (Update _) = False


requestBot :: Integer -> IO ()
requestBot offset = do
    json <- fetchJSON offset
    let eitherResponse = eitherDecode (BSL.fromStrict json) :: Either String Response
    print eitherResponse
    case eitherResponse of (Left err) -> print err
                           (Right Response {result = updates}) ->
                               do
                                mapM_ processUpdate $ filter isMessageUpdate updates
                                if null updates
                                    then requestBot offset
                                    else requestBot $ (+ fromIntegral (length updates)) . getUpdateId . last $ updates

