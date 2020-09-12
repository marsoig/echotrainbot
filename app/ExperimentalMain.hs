{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

import Lib

import           GHC.Generics
import           Network.HTTP.Conduit           (simpleHttp)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy.Internal  as BSL
import           Control.Monad.Trans.Maybe
import           Data.Maybe                     (fromMaybe)
import           Data.ByteString                as BS
--import qualified Data.ByteString.Char8         as BS
--import         Data.Text                      ( Text )
import           Data.Text.Lazy                as T (Text, concat, pack, unpack)
--import qualified Data.Text.Lazy.IO             as T
--import qualified Data.Text.Lazy.Encoding       as T
import           Data.Maybe                    (fromJust)


data User = User {  id                          :: Integer
                  , is_bot                      :: Bool
                  , first_name                  :: Text
                  , last_name                   :: Maybe Text
                  , username                    :: Maybe Text
                  , language_code               :: Maybe Text
                  , can_join_groups             :: Maybe Text
                  , can_read_all_group_messages :: Maybe Text
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
                       , results :: [Results]} deriving (Show, Eq, Generic)

instance FromJSON Updates

data PhotoSize = PhotoSize {  file_id        :: String
                            , file_unique_id :: String
                            , width          :: Integer
                            , height         :: Integer
                            , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON PhotoSize

data Sticker = Sticker {  file_id        :: Text
                        , file_unique_id :: Text
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


data MessageType = MText | MSticker | MAnimation | MAudio | MDocument | MVideo | MVideoNote | MVoice | MContact | MPhoto | MError

checkMessageType :: Message -> MessageType
checkMessageType msg = if | (text $ msg) /= Nothing -> MText
                          | (sticker $ msg) /= Nothing -> MSticker
                          | (animation $ msg) /= Nothing -> MAnimation
                          | (audio $ msg) /= Nothing -> MAudio
                          | (document $ msg) /= Nothing -> MDocument
                          | (video $ msg) /= Nothing -> MVideo
                          | (video_note $ msg) /= Nothing -> MVideoNote
                          | (voice $ msg) /= Nothing -> MVoice
                          | (contact $ msg) /= Nothing -> MContact
                          | (photo $ msg) /= Nothing -> MPhoto
                          | otherwise -> MError

urlToken = "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8"

decodeToUpdates :: BSL.ByteString -> Updates
decodeToUpdates x = case decode x :: Maybe Updates of
                      Just u -> u
                      Nothing -> Updates { ok = False, results = []}

checkUpdates :: BSL.ByteString -> Integer
checkUpdates bts = if null (results $ (decodeToUpdates bts)) then 0
                    else (update_id $ last $ results $ (decodeToUpdates bts))

makeNewUpdateRequest :: BSL.ByteString -> String
makeNewUpdateRequest bts = if checkUpdates bts == 0 then urlToken ++ "/getUpdates?timeout=3000"
                                 else urlToken ++ "/getUpdates?offset=" ++ show (checkUpdates bts) ++ "&timeout=3000"
data SendingSet = SendingSet {command :: String, parameters :: String}

addMessageParameters :: Message -> SendingSet
addMessageParameters msg = case (checkMessageType msg) of
                                 MText -> SendingSet {command = "/sendMessage", parameters = ("&text=" ++ (fromJust $ text $ msg))}
                                 MSticker -> SendingSet {command = "/sendSticker", parameters = ("&sticker=" ++ (file_id $ fromJust $ sticker $ msg))}
                                 MAnimation -> SendingSet {command = "/sendAnimation", parameters = ("&animation=" ++ (file_id $ fromJust $ animation $ msg))}
                                 MAudio -> SendingSet {command = "/sendAudio", parameters = ("&audio=" ++ (file_id $ fromJust $ audio $ msg))}
                                 MDocument -> SendingSet {command = "/sendDocument", parameters = ("&document=" ++ (file_id $ fromJust $ document $ msg))}
                                 MVideo -> SendingSet {command = "/sendVideo", parameters = ("&video=" ++ (file_id $ fromJust $ video $ msg))}
                                 MVideoNote -> SendingSet {command = "/sendVideoNote", parameters = ("&video_note=" ++ (file_id $ fromJust $ video_note $ msg))}
                                 MVoice -> SendingSet {command = "/sendVoice", parameters = ("&voice=" ++ (file_id $ fromJust $ voice $ msg))}
                                 MContact -> SendingSet {command = "/sendContact", parameters = ("&phone_number=" ++ (phone_number $ fromJust $ contact $ msg) ++ "&first_name=" ++ (first_name $ fromJust $ contact $ msg))}
                                 MPhoto -> SendingSet {command = "/sendPhoto", parameters = ("&photo=" ++ (file_id $ head $ fromJust $ photo $ msg))}
                                 MError -> SendingSet {command = "/sendMessage", parameters = ("&text=Error with message type")}

makeURLRequest :: Message -> SendingSet -> String
makeURLRequest msg sendset = urlToken ++ (command $ sendset) ++ "?chat_id=" ++ (show $ chat_id $ chat $ msg) ++ (parameters $ sendset) ++ cap
                              where cap = case (caption $ msg) of
                                            Nothing -> ""
                                            Just x -> "&caption=" ++ x

sendAnswerOrNot :: ByteString -> IO (ByteString)
sendAnswerOrNot bst = if checkUpdates bst == 0 then pure (BS.empty) :: IO (ByteString)
                          else simpleHttp address where
                             address = makeUrlRequest msg (addMessageParameters msg) where
                                msg = message $ last $ results $ (decodeToUpdates bst)


getUpdates :: String -> IO (Updates)
getUpdates adr = do
    download <- simpleHttp adr
    answer <- sendAnswerOrNot download
    newQuery <- getUpdates (makeNewUpdateRequest download)
    return newQuery
