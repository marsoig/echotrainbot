{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Main where
import Lib

import           Text.Read                      (readMaybe)
import           ConfigParser
import           System.Log.Logger
import           GHC.Generics
import           Network.HTTP.Conduit           (simpleHttp)
import           Data.Aeson
import           Control.Monad.Trans.Maybe
import           Data.Maybe                     (fromMaybe, fromJust)
import           Data.ByteString.Lazy           as BSL (empty, ByteString)
--import qualified Data.ByteString.Char8         as BS
--import         Data.Text                      ( Text )
import           Data.Text.Lazy                 as T (Text, concat, pack, unpack)
--import qualified Data.Text.Lazy.IO             as T


data User = User {  id                          :: Integer
                  , is_bot                      :: Bool
                  , first_name                  :: String
                  , last_name                   :: Maybe String
                  , username                    :: Maybe String
                  , language_code               :: Maybe String
                  , can_join_groups             :: Maybe String
                  , can_read_all_group_messages :: Maybe String
                  , supports_inline_queries     :: Maybe Bool  } deriving (Show, Eq, Generic)

instance FromJSON User

data Chat = Chat {  chat_id         :: Integer} deriving (Show, Eq, Generic)

instance FromJSON Chat where
  parseJSON = withObject "chat" $ \o -> do
    chat_id                      <- o.: "id"
    return Chat {..}

data Message = Message {  message_id :: Integer
                        , from       :: Maybe User
                        , date       :: Integer
                        , chat       :: Chat
                        , text       :: Maybe String
                        , sticker    :: Maybe Sticker
                        , animation  :: Maybe Animation
                        , audio      :: Maybe Audio
                        , document   :: Maybe Document
                        , video      :: Maybe Video
                        , video_note :: Maybe VideoNote
                        , voice      :: Maybe Voice
                        , contact    :: Maybe Contact
                        , caption    :: Maybe String
                        , photo      :: Maybe [PhotoSize]} deriving (Show, Eq, Generic)

instance FromJSON Message

data Results = Results {  update_id :: Integer
                        , message   :: Message} deriving (Show, Eq, Generic)

instance FromJSON Results
data Updates = Updates { ok      :: Bool
                       , result :: [Results]} deriving (Show, Eq, Generic)

instance FromJSON Updates

data PhotoSize = PhotoSize {  file_id        :: String
                            , file_unique_id :: String
                            , width          :: Integer
                            , height         :: Integer
                            , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON PhotoSize

data Sticker = Sticker {  file_id        :: String
                        , file_unique_id :: String
                        , width          :: Integer
                        , height         :: Integer
                        , is_animated    :: Bool
                        , emoji          :: Maybe String
                        , set_name       :: Maybe String
                        , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Sticker

data Animation = Animation {  file_id        :: String
                            , file_unique_id :: String
                            , width          :: Integer
                            , height         :: Integer
                            , duration       :: Integer
                            , thumb          :: Maybe PhotoSize
                            , file_name      :: Maybe String
                            , mime_type      :: Maybe String
                            , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Animation

data Audio = Audio {  file_id        :: String
                    , file_unique_id :: String
                    , duration       :: Integer
                    , performer            :: Maybe String
                    , title                :: Maybe String
                    , mime_type            :: Maybe String
                    , file_size            :: Maybe Integer
                    , thumb          :: Maybe PhotoSize} deriving (Show, Eq, Generic)

instance FromJSON Audio

data Document = Document {  file_id        :: String
                          , file_unique_id :: String
                          , thumb          :: Maybe PhotoSize
                          , file_name               :: Maybe String
                          , mime_type               :: Maybe String
                          , file_size               :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Document

data Video = Video {  file_id        :: String
                    , file_unique_id :: String
                    , width          :: Integer
                    , height         :: Integer
                    , duration       :: Integer
                    , thumb          :: Maybe PhotoSize
                    , mime_type      :: Maybe String
                    , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Video

data VideoNote = VideoNote {  file_id        :: String
                            , file_unique_id :: String
                            , length         :: Integer
                            , duration       :: Integer
                            , thumb          :: Maybe PhotoSize
                            , file_size                 :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON VideoNote

data Voice = Voice {  file_id        :: String
                    , file_unique_id :: String
                    , duration       :: Integer
                    , mime_type      :: Maybe String
                    , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Voice

data Contact = Contact {  phone_number :: String
                        , first_name   :: String
                        , last_name    :: Maybe String
                        , user_id      :: Maybe Integer
                        , vcard        :: Maybe String} deriving (Show, Eq, Generic)

instance FromJSON Contact

data SendingSet = SendingSet {command :: String, parameters :: String}

analyzeMessage :: Message -> SendingSet
analyzeMessage (Message {text = Just x})                                                = SendingSet {command = "/sendMessage", parameters = "&text=" ++ x}
analyzeMessage (Message {sticker = Just (Sticker {file_id = x}) })                      = SendingSet {command = "/sendSticker", parameters = "&sticker=" ++ x}
analyzeMessage (Message {animation = Just (Animation {file_id = x}) })                  = SendingSet {command = "/sendAnimation", parameters = "&animation=" ++ x}
analyzeMessage (Message {audio = Just (Audio {file_id = x}) })                          = SendingSet {command = "/sendAudio", parameters = "&audio=" ++ x}
analyzeMessage (Message {document = Just (Document {file_id = x}) })                    = SendingSet {command = "/sendDocument", parameters = "&document=" ++ x}
analyzeMessage (Message {video = Just (Video {file_id = x}) })                          = SendingSet {command = "/sendVideo", parameters = "&video=" ++ x}
analyzeMessage (Message {video_note = Just (VideoNote {file_id = x}) })                 = SendingSet {command = "/sendVideoNote", parameters = "&video_note=" ++ x}
analyzeMessage (Message {voice = Just (Voice {file_id = x}) })                          = SendingSet {command = "/sendVoice", parameters = "&voice=" ++ x}
analyzeMessage (Message {contact = Just (Contact {phone_number = p, first_name = f}) }) = SendingSet {command = "/sendContact", parameters ="&phone_number=" ++ p ++ "&first_name=" ++ f}
analyzeMessage (Message {photo = Just photolist})                                       = SendingSet {command = "/sendPhoto", parameters = "&photo=" ++ getFileIdPhoto (last $ photolist)}
                                                                                   where getFileIdPhoto (PhotoSize {file_id = f}) = f


urlToken = "https://api.telegram.org/bot"

decodeToUpdates :: ByteString -> Updates
decodeToUpdates x = case decode x :: Maybe Updates of
                      Just u -> u
                      Nothing -> Updates { ok = False, result = []}

updatesRequest :: ByteString -> String -> String
updatesRequest upd token = if null (result $ (decodeToUpdates upd)) then "https://api.telegram.org/bot" ++ token ++ "/getUpdates?timeout=3000"
                            else  "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ lastMessageId ++ "&timeout=3000"
                             where lastMessageId = show ((update_id $ last $ result $ (decodeToUpdates upd)) +1)

makeAnswerRequest :: Message -> String -> SendingSet -> String
makeAnswerRequest msg token sendset = "https://api.telegram.org/bot" ++ token ++ (command $ sendset) ++ "?chat_id=" ++ (show $ chat_id $ chat $ msg) ++ (parameters $ sendset) ++ cap
                              where cap = case (caption $ msg) of
                                            Nothing -> ""
                                            Just x -> "&caption=" ++ x

checkUpdates :: ByteString -> IO ()
checkUpdates x = case decode x :: Maybe Updates of
                   Nothing -> errorM "Logging.Main" "Could not parse updates"
                   Just (Updates {ok = True, result = []}) -> infoM "Logging.Main" "No new messages"
                   _ -> infoM  "Logging.Main" "New message encountered"

sendAnswer :: ByteString -> String -> IO (ByteString)
sendAnswer upd token = simpleHttp (makeAnswerRequest msg token (analyzeMessage msg))
  where msg = message $ last $ result $ (decodeToUpdates upd)

checkConfig :: Config -> IO ()
checkConfig config = if telegramToken config == "" then errorM "Logging.Main" "Telegram token was not found. Please write telegram token in the file config.txt"
                      else
                        if (readMaybe (logLevel config) :: Maybe Priority) == Nothing then warningM "Logging.Main" "Could not read logging level from config.txt. Logging level is set as DEBUG by default"
                          else infoM "Logging.Main" "Configuration file was parsed"

botLoop :: Integer -> String -> IO (String)
botLoop messageId token = do
  infoM "LoggingExample.Main" ("BotLoop is started, id=" ++ show messageId)
  updates <- case messageId of
                      0 -> simpleHttp ("https://api.telegram.org/bot" ++ token ++ "/getUpdates?timeout=3000")
                      _ -> simpleHttp ("https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show messageId ++ "&timeout=3000")
  checkUpdates updates
  case decode updates :: Maybe Updates of
    Just (Updates {ok = True, result = []}) -> return ()
    Just (Updates {ok = True, result = x}) -> do {sendAnswer updates token;
                                                  messageId <- pure ((update_id $ last $ x) + 1) :: IO (Integer);
                                                  debugM "Logging.Main" ("Answer on message " ++ show messageId ++ " was sent")}
    _ -> error "Could not parse updates"
  messageId <- case decode updates :: Maybe Updates of
    Just (Updates {ok = True, result = []}) -> pure 0 :: IO (Integer)
    Just (Updates {ok = True, result = x}) -> do pure ((update_id $ last $ x) + 1) :: IO (Integer)
    _ -> error "Could not parse updates"
  botLoop messageId token
  return "end"

main:: IO (String)
main = do
  config <- readFile "config.txt" >>= ((\x -> pure x :: IO Config).parseConfig)
  --config <- pure (parseConfig configText) :: IO Config
  checkConfig config
  mainTest
  token <- pure (telegramToken config) :: IO String
  updateGlobalLogger "LoggingExample.Main" (setLevel (read (logLevel config) :: Priority))
  infoM "LoggingExample.Main" "EchoBot is started"
  botLoop 0 token
  return "end"
