{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import Lib

import           GHC.Generics
import           Network.HTTP.Conduit           (simpleHttp)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy.Internal  as BSL
import           Control.Monad.Trans.Maybe
import           Data.Maybe                     (fromMaybe)
--import qualified Data.ByteString.Char8         as BS
--import         Data.Text                      ( Text )
import           Data.Text.Lazy                as T (Text, concat, pack, unpack)
--import qualified Data.Text.Lazy.IO             as T
--import qualified Data.Text.Lazy.Encoding       as T
import           Data.Maybe                    (fromJust)


data User = User {  userId                      :: Integer
                  , is_bot                      :: Bool
                  , user_first_name                  :: Text
                  , last_name                   :: Maybe Text
                  , username                    :: Maybe Text
                  , language_code               :: Maybe Text
                  , can_join_groups             :: Maybe Text
                  , can_read_all_group_messages :: Maybe Text
                  , supports_inline_queries     :: Maybe Bool  } deriving (Show, Eq, Generic)

instance FromJSON User where
 parseJSON = withObject "user" $ \o -> do
   userId                      <- o.: "id"
   is_bot                      <- o.: "is_bot"
   user_first_name             <- o.: "first_name"
   last_name                   <- o.:? "last_name"
   username                    <- o.:? "username"
   language_code               <- o.:? "language_code"
   can_join_groups             <- o.:? "can_join_groups"
   can_read_all_group_messages <- o.:? "can_read_all_group_messages"
   supports_inline_queries     <- o.:? "supports_inline_queries"
   return User {..}

data Chat = Chat { chatId      :: Integer
                  , type'      :: Text
                  , title      :: Maybe Text
                  , username   :: Maybe Text
                  , first_name :: Maybe Text
                  , last_name  :: Maybe Text} deriving (Show, Eq, Generic)

instance FromJSON Chat where
 parseJSON = withObject "chat" $ \o -> do
   chatId                      <- o.: "id"
   type'                       <- o.: "type"
   title                       <- o.:? "title"
   username                    <- o.:? "username"
   first_name                  <- o.:? "first_name"
   last_name                   <- o.:? "last_name"
   return Chat {..}

data Message = Message {  message_id :: Integer
                        , from       :: Maybe User
                        , date       :: Integer
                        , chat       :: Chat
                        , text       :: Maybe Text
                        , sticker    :: Maybe Sticker
                        , animation  :: Maybe Animation
                        , audio      :: Maybe Audio
                        , document   :: Maybe Document
                        , video      :: Maybe Video
                        , video_note :: Maybe VideoNote
                        , voice      :: Maybe Voice
                        , contact    :: Maybe Contact
                        , caption    :: Maybe Text
                        , photo      :: Maybe Photo} deriving (Show, Eq, Generic)

instance FromJSON Message where
 parseJSON = withObject "message" $ \o -> do
   message_id                 <- o.: "message_id"
   from                       <- o.:? "from"
   date                       <- o.: "date"
   chat                       <- o.: "chat"
   text                       <- o.:? "text"
   sticker                    <- o.:? "sticker"
   animation                  <- o.:? "animation"
   audio                      <- o.:? "audio"
   document                   <- o.:? "document"
   video                      <- o.:? "video"
   video_note                 <- o.:? "video_note"
   voice                      <- o.:? "voice"
   contact                    <- o.:? "contact"
   caption                    <- o.:? "caption"
   photo                      <- o.:? "photo"
   return Message {..}

data Results = Results {  update_id :: Integer
                        , message   :: Message} deriving (Show, Eq, Generic)

instance FromJSON Results where
 parseJSON = withObject "result" $ \o -> do
   update_id                 <- o.: "update_id"
   message                   <- o.: "message"
   return Results {..}

data Updates = Updates { ok      :: Bool
                       , results :: [Results]} deriving (Show, Eq, Generic)

instance FromJSON Updates where
 parseJSON = withObject "updates" $ \o -> do
   ok                         <- o.:  "ok"
   results                    <- o.:  "result"
   return Updates {..}

data PhotoSize = PhotoSize {  file_id        :: String
                            , file_unique_id :: String
                            , width          :: Integer
                            , height         :: Integer
                            , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON PhotoSize

data Sticker = Sticker {  sticker_file_id        :: Text
                        , sticker_file_unique_id :: Text
                        , sticker_width          :: Integer
                        , sticker_height         :: Integer
                        , is_animated    :: Bool
                        , emoji          :: Maybe String
                        , set_name       :: Maybe String
                        , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Sticker where
 parseJSON = withObject "sticker" $ \o -> do
   sticker_file_id                      <- o.: "file_id"
   sticker_file_unique_id               <- o.: "file_unique_id"
   sticker_width                        <- o.: "width"
   sticker_height                       <- o.: "height"
   is_animated                  <- o.: "is_animated"
   emoji                        <- o.:? "emoji"
   set_name                     <- o.:? "set_name"
   file_size                    <- o.:? "file_size"
   return Sticker {..}

data Animation = Animation {  animation_file_id        :: String
                            , animation_file_unique_id :: String
                            , animation_width          :: Integer
                            , animation_height         :: Integer
                            , animation_duration       :: Integer
                            , animation_thumb          :: Maybe PhotoSize
                            , file_name      :: Maybe String
                            , mime_type      :: Maybe String
                            , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Animation where
  parseJSON = withObject "animation" $ \o -> do
    animation_file_id        <- o.: "file_id"
    animation_file_unique_id <- o.: "file_unique_id"
    animation_width          <- o.: "width"
    animation_height         <- o.: "height"
    animation_duration       <- o.: "duration"
    animation_thumb          <- o.:? "thumb"
    file_name                <- o.:? "file_name"
    mime_type                <- o.:? "mime_type"
    file_size                <- o.:? "file_size"
    return Animation {..}

data Audio = Audio {  audio_file_id        :: String
                    , audio_file_unique_id :: String
                    , audio_duration       :: Integer
                    , performer            :: Maybe String
                    , title                :: Maybe String
                    , mime_type            :: Maybe String
                    , file_size            :: Maybe Integer
                    , audio_thumb          :: Maybe PhotoSize} deriving (Show, Eq, Generic)

instance FromJSON Audio where
  parseJSON = withObject "audio" $ \o -> do
    audio_file_id        <- o.: "file_id"
    audio_file_unique_id <- o.: "file_unique_id"
    audio_duration       <- o.: "duration"
    audio_thumb          <- o.:? "thumb"
    performer            <- o.:? "performer"
    title                <- o.:? "title"
    mime_type            <- o.:? "mime_type"
    file_size            <- o.:? "file_size"
    return Audio {..}

data Document = Document {  document_file_id        :: String
                          , document_file_unique_id :: String
                          , document_thumb          :: Maybe PhotoSize
                          , file_name               :: Maybe String
                          , mime_type               :: Maybe String
                          , file_size               :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Document where
  parseJSON = withObject "document" $ \o -> do
    document_file_id        <- o.: "file_id"
    document_file_unique_id <- o.: "file_unique_id"
    document_thumb          <- o.:? "thumb"
    file_name               <- o.:? "file_name"
    mime_type               <- o.:? "mime_type"
    file_size               <- o.:? "file_size"
    return Document {..}

data Video = Video {  video_file_id        :: String
                    , video_file_unique_id :: String
                    , video_width          :: Integer
                    , video_height         :: Integer
                    , video_duration       :: Integer
                    , video_thumb          :: Maybe PhotoSize
                    , mime_type      :: Maybe String
                    , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Video where
  parseJSON = withObject "video" $ \o -> do
    video_file_id        <- o.: "file_id"
    video_file_unique_id <- o.: "file_unique_id"
    video_width          <- o.: "width"
    video_height         <- o.: "height"
    video_duration       <- o.: "duration"
    video_thumb          <- o.:? "thumb"
    mime_type            <- o.:? "mime_type"
    file_size            <- o.:? "file_size"
    return Video {..}

data VideoNote = VideoNote {  video_note_file_id        :: String
                            , video_note_file_unique_id :: String
                            , video_note_length         :: Integer
                            , video_note_duration       :: Integer
                            , video_note_thumb          :: Maybe PhotoSize
                            , file_size                 :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON VideoNote where
  parseJSON = withObject "videonote" $ \o -> do
    video_note_file_id        <- o.: "file_id"
    video_note_file_unique_id <- o.: "file_unique_id"
    video_note_duration       <- o.: "duration"
    video_note_length         <- o.: "length"
    video_note_thumb          <- o.:? "thumb"
    file_size                 <- o.:? "file_size"
    return VideoNote {..}

data Voice = Voice {  voice_file_id        :: String
                    , voice_file_unique_id :: String
                    , voice_duration       :: Integer
                    , mime_type      :: Maybe String
                    , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

instance FromJSON Voice where
  parseJSON = withObject "voice" $ \o -> do
    voice_file_id        <- o.: "file_id"
    voice_file_unique_id <- o.: "file_unique_id"
    voice_duration       <- o.: "duration"
    mime_type               <- o.:? "mime_type"
    file_size               <- o.:? "file_size"
    return Voice {..}

data Contact = Contact {  phone_number :: String
                        , contact_first_name   :: String
                        , last_name    :: Maybe String
                        , user_id      :: Maybe Integer
                        , vcard        :: Maybe String} deriving (Show, Eq, Generic)

instance FromJSON Contact where
  parseJSON = withObject "contact" $ \o -> do
    phone_number        <- o.: "phone_number"
    contact_first_name  <- o.: "contact_first_name"
    last_name           <- o.:? "last_name"
    user_id             <- o.:? "user_id"
    vcard               <- o.:? "vcard "
    return Contact {..}

data Photo = Photo {photo_list :: [PhotoSize]} deriving (Show, Eq, Generic)

instance FromJSON Photo


getUpdates :: String -> IO (Updates)
getUpdates adr = do
    download <- simpleHttp adr
    case decode download :: Maybe Updates of
                   Just x  -> return x
                   Nothing -> return $ Updates { ok = False, results = []}

sendText :: Message -> IO (Text)
sendText userMessage = do
    messageText     <- pure (fromMaybe (pack []) (text $ userMessage)) :: IO Text
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendMessage?chat_id=", pack $ show messageChatId, pack "&text=", messageText]) :: IO Text
    return sendingQuery

sendSticker :: Message -> IO (Text)
sendSticker userMessage = do
    sticker_id      <- pure (sticker_file_id $ fromJust (sticker $ userMessage)) :: IO Text
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendSticker?chat_id=", pack $ show messageChatId, pack "&sticker=", sticker_id]) :: IO Text
    return sendingQuery

sendAnimation :: Message -> IO (Text)
sendAnimation userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    animation_id    <- pure (animation_file_id $ fromJust (animation $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendAnimation?chat_id=", pack (show messageChatId), pack "&animation=", pack animation_id]) :: IO Text
    return sendingQuery

sendAudio :: Message -> IO (Text)
sendAudio userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    audio_id        <- pure (audio_file_id $ fromJust (audio $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendAudio?chat_id=", pack (show messageChatId), pack "&audio=", pack audio_id]) :: IO Text
    return sendingQuery

sendDocument :: Message -> IO (Text)
sendDocument userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    document_id        <- pure (document_file_id $ fromJust (document $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendDocument?chat_id=", pack (show messageChatId), pack "&document=", pack document_id]) :: IO Text
    return sendingQuery

sendVideo :: Message -> IO (Text)
sendVideo userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    video_id        <- pure (video_file_id $ fromJust (video $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendVideo?chat_id=", pack (show messageChatId), pack "&video=", pack video_id]) :: IO Text
    return sendingQuery

sendVideoNote :: Message -> IO (Text)
sendVideoNote userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    video_note_id   <- pure (video_note_file_id $ fromJust (video_note $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendVideoNote?chat_id=", pack (show messageChatId), pack "&video_note=", pack video_note_id]) :: IO Text
    return sendingQuery

sendVoice :: Message -> IO (Text)
sendVoice userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    voice_id        <- pure (voice_file_id $ fromJust (voice $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendVoice?chat_id=", pack (show messageChatId), pack "&voice=", pack voice_id]) :: IO Text
    return sendingQuery

sendPhoto :: Message -> IO (Text)
sendPhoto userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    photo_id        <- pure ( file_id $ head $ photo_list $ fromJust (photo $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendPhoto?chat_id=", pack (show messageChatId), pack "&photo=", pack photo_id]) :: IO Text
    return sendingQuery

sendContact :: Message -> IO (Text)
sendContact userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    phone_number    <- pure (phone_number $ fromJust (contact $ userMessage)) :: IO String
    first_name      <- pure (contact_first_name $ fromJust (contact $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendVoice?chat_id=", pack (show messageChatId), pack "&phone_number=", pack phone_number, pack "&first_name=", pack first_name]) :: IO Text
    return sendingQuery

waitForUpdates :: String -> IO (Updates)
waitForUpdates adr = do
    receivedUpdates <- getUpdates adr
    if null (results $ receivedUpdates) then waitForUpdates adr
                                      else return receivedUpdates

mainP :: Integer -> IO (String)
mainP updateId = do
    address <- if updateId == 0 then (pure ("https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/getUpdates?timeout=300") :: IO String)
                   else (pure ("https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/getUpdates?offset=" ++ (show updateId) ++ "&timeout=3000") :: IO String)
    receivedUpdates <- waitForUpdates address
    lastMessage <- pure (message.last.results $ receivedUpdates) :: IO Message
    lastUpdateId <- pure (update_id.last.results $ receivedUpdates) :: IO Integer
    sendingQuery <-
      if (text $ lastMessage) /= Nothing then sendText lastMessage
       else  if (sticker $ lastMessage) /= Nothing then sendSticker lastMessage
        else if (animation $ lastMessage) /= Nothing then sendAnimation lastMessage
          else  if (audio $ lastMessage) /= Nothing then sendAudio lastMessage
            else if (document $ lastMessage) /= Nothing then sendDocument lastMessage
              else  if (video $ lastMessage) /= Nothing then sendVideo lastMessage
                else if (video_note $ lastMessage) /= Nothing then sendVideoNote lastMessage
                  else  if (voice $ lastMessage) /= Nothing then sendVoice lastMessage
                    else if (contact $ lastMessage) /= Nothing then sendContact lastMessage
                      else if (photo $ lastMessage) /= Nothing then sendPhoto lastMessage
                        else return ("Success")
    capSendingQuery <- (\x -> pure x :: IO (String)) (if (caption $ lastMessage) /= Nothing then (unpack sendingQuery) ++ "&caption=" ++ (unpack (fromJust $ caption $ lastMessage)) else (unpack sendingQuery))
    sending <- simpleHttp capSendingQuery
    mainP (lastUpdateId+1)
