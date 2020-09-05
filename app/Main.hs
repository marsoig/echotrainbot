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
                  , first_name                  :: Text
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
   first_name                  <- o.: "first_name"
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
                        , animation  :: Maybe Animation} deriving (Show, Eq, Generic)

instance FromJSON Message where
 parseJSON = withObject "message" $ \o -> do
   message_id                 <- o.: "message_id"
   from                       <- o.:? "from"
   date                       <- o.: "date"
   chat                       <- o.: "chat"
   text                       <- o.:? "text"
   sticker                    <- o.:? "sticker"
   animation                  <- o.:? "animation"
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
    animation_width <- o.: "width"
    animation_height <- o.: "height"
    animation_duration <- o.: "duration"
    animation_thumb <- o.:? "thumb"
    file_name <- o.:? "file_name"
    mime_type <- o.:? "mime_type"
    file_size <- o.:? "file_size"
    return Animation {..}

data Audio = Audio {  file_id        :: String
                    , file_unique_id :: String
                    , duration       :: Integer
                    , performer      :: Maybe String
                    , title          :: Maybe String
                    , mime_type      :: Maybe String
                    , file_size      :: Maybe Integer
                    , thumb          :: Maybe PhotoSize} deriving (Show, Eq, Generic)

data Document = Document {  file_id        :: String
                          , file_unique_id :: String
                          , thumb          :: Maybe PhotoSize
                          , file_name      :: Maybe String
                          , mime_type      :: Maybe String
                          , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

data Video = Video {  file_id        :: String
                    , file_unique_id :: String
                    , width          :: Integer
                    , height         :: Integer
                    , duration       :: Integer
                    , thumb          :: Maybe PhotoSize
                    , mime_type      :: Maybe String
                    , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

data VideoNote = VideoNote {  file_id        :: String
                            , file_unique_id :: String
                            , length         :: Integer
                            , duration       :: Integer
                            , thumb          :: Maybe PhotoSize
                            , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

data Voice = Voice {  file_id        :: String
                    , file_unique_id :: String
                    , duration       :: Integer
                    , mime_type      :: Maybe String
                    , file_size      :: Maybe Integer} deriving (Show, Eq, Generic)

data Contact = Contact {  phone_number :: String
                        , first_name   :: String
                        , last_name    :: Maybe String
                        , user_id      :: Maybe Integer
                        , vcard        :: Maybe String} deriving (Show, Eq, Generic)



getUpdates :: String -> IO (Updates)
getUpdates adr = do
    download <- simpleHttp adr
    case decode download :: Maybe Updates of
                   Just x  -> return x
                   Nothing -> return $ Updates { ok = False, results = []}

makeTextAnswer :: Message -> IO (Text)
makeTextAnswer userMessage = do
    messageText     <- pure (fromMaybe (pack []) (text $ userMessage)) :: IO Text
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendMessage?chat_id=", pack $ show messageChatId, pack "&text=", messageText]) :: IO Text
    check           <- simpleHttp (unpack sendingQuery)
    return sendingQuery

makeStickerAnswer :: Message -> IO (Text)
makeStickerAnswer userMessage = do
    sticker_id      <- pure (sticker_file_id $ fromJust (sticker $ userMessage)) :: IO Text
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendSticker?chat_id=", pack $ show messageChatId, pack "&sticker=", sticker_id]) :: IO Text
    check           <- simpleHttp (unpack sendingQuery)
    return sendingQuery

makeAnimationAnswer :: Message -> IO (Text)
makeAnimationAnswer userMessage = do
    messageChatId   <- pure (chatId.chat $ userMessage) :: IO Integer
    animation_id    <- pure (animation_file_id $ fromJust (animation $ userMessage)) :: IO String
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendAnimation?chat_id=", pack (show messageChatId), pack "&animation=", pack animation_id]) :: IO Text
    check           <- simpleHttp (unpack sendingQuery)
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
    if (text $ lastMessage) /= Nothing then makeTextAnswer lastMessage
      else  if (sticker $ lastMessage) /= Nothing then makeStickerAnswer lastMessage
        else if (animation $ lastMessage) /= Nothing then makeAnimationAnswer lastMessage else return ("Success")
    mainP (lastUpdateId+1)
