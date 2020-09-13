{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

import Lib

import           GHC.Generics
import           Network.HTTP.Conduit           (simpleHttp)
import           Data.Aeson
import           Control.Monad.Trans.Maybe
import           Data.Maybe                     (fromMaybe)
import           Data.ByteString.Lazy                as BSL (empty, ByteString)
--import qualified Data.ByteString.Char8         as BS
--import         Data.Text                      ( Text )
import           Data.Text.Lazy                as T (Text, concat, pack, unpack)
--import qualified Data.Text.Lazy.IO             as T
--import qualified Data.Text.Lazy.Encoding       as T
import           Data.Maybe                    (fromJust)


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


urlToken = "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8"

decodeToUpdates :: ByteString -> Updates
decodeToUpdates x = case decode x :: Maybe Updates of
                      Just u -> u
                      Nothing -> Updates { ok = False, result = []}

checkUpdates :: ByteString -> Integer
checkUpdates bts = if null (result $ (decodeToUpdates bts)) then 0
                    else (update_id $ last $ result $ (decodeToUpdates bts))

makeNewUpdateRequest :: ByteString -> String
makeNewUpdateRequest bts = if checkUpdates bts == 0 then urlToken ++ "/getUpdates?timeout=3000"
                                 else urlToken ++ "/getUpdates?offset=" ++ show (checkUpdates (bts) +1) ++ "&timeout=3000"
data SendingSet = SendingSet {command :: String, parameters :: String}

func :: Message -> SendingSet
func (Message {text = Just x})                                                = SendingSet {command = "/sendMessage", parameters = "&text=" ++ x}
func (Message {sticker = Just (Sticker {file_id = x}) })                      = SendingSet {command = "/sendSticker", parameters = "&sticker=" ++ x}
func (Message {animation = Just (Animation {file_id = x}) })                  = SendingSet {command = "/sendAnimation", parameters = "&animation=" ++ x}
func (Message {audio = Just (Audio {file_id = x}) })                          = SendingSet {command = "/sendAudio", parameters = "&audio=" ++ x}
func (Message {document = Just (Document {file_id = x}) })                    = SendingSet {command = "/sendDocument", parameters = "&document=" ++ x}
func (Message {video = Just (Video {file_id = x}) })                          = SendingSet {command = "/sendVideo", parameters = "&video=" ++ x}
func (Message {video_note = Just (VideoNote {file_id = x}) })                 = SendingSet {command = "/sendVideoNote", parameters = "&video_note=" ++ x}
func (Message {voice = Just (Voice {file_id = x}) })                          = SendingSet {command = "/sendVoice", parameters = "&voice=" ++ x}
func (Message {contact = Just (Contact {phone_number = p, first_name = f}) }) = SendingSet {command = "/sendContact", parameters ="&phone_number=" ++ p ++ "&first_name=" ++ f}
func (Message {photo = Just photolist})                                       = SendingSet {command = "/sendPhoto", parameters = "&photo=" ++ getFileIdPhoto (last $ photolist)}
                                                                                   where getFileIdPhoto (PhotoSize {file_id = f}) = f



makeURLRequest :: Message -> SendingSet -> String
makeURLRequest msg sendset = urlToken ++ (command $ sendset) ++ "?chat_id=" ++ (show $ chat_id $ chat $ msg) ++ (parameters $ sendset) ++ cap
                              where cap = case (caption $ msg) of
                                            Nothing -> ""
                                            Just x -> "&caption=" ++ x

sendAnswerOrNot :: ByteString -> IO (ByteString)
sendAnswerOrNot bst = if checkUpdates bst == 0 then pure (BSL.empty) :: IO (BSL.ByteString)
                          else simpleHttp address where
                             address = makeURLRequest msg (func msg) where
                                msg = message $ last $ result $ (decodeToUpdates bst)


getUpdates :: String -> IO (String)
getUpdates adr = do
    download <- simpleHttp adr
    answer <- sendAnswerOrNot download
    check <- pure (decodeToUpdates download) :: IO (Updates)
    newQuery <- getUpdates (makeNewUpdateRequest download)
    chup <- pure (makeNewUpdateRequest download) :: IO (String)
    return chup
