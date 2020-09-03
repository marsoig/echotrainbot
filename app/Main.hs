
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}


import Lib

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


address = "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/getUpdates"
data User = User {  userId                          :: Integer
                  , is_bot                      :: Bool
                  , first_name                  :: Text
                  , last_name                   :: Maybe Text
                  , username                    :: Maybe Text
                  , language_code               :: Maybe Text
                  , can_join_groups             :: Maybe Text
                  , can_read_all_group_messages :: Maybe Text
                  , supports_inline_queries     :: Maybe Bool  } deriving (Show)

data Chat = Chat { chatId      :: Integer
                  , type'      :: Text
                  , title      :: Maybe Text
                  , username   :: Maybe Text
                  , first_name :: Maybe Text
                  , last_name  :: Maybe Text} deriving (Show)

data Message = Message {  message_id :: Integer
                        , from       :: Maybe User
                        , date       :: Integer
                        , chat       :: Chat
                        , text       :: Maybe Text
                        , sticker    :: Maybe Sticker} deriving (Show)

data Results = Results {  update_id :: Integer
                        , message   :: Message} deriving (Show)

data Updates = Updates { ok :: Bool
                       , results :: [Results]} deriving (Show)

data Sticker = Sticker {  file_id :: Text
                        , file_unique_id :: Text
                        , width :: Integer
                        , height :: Integer
                        , is_animated :: Bool
                        , emoji :: Maybe String
                        , set_name :: Maybe String
                        , file_size :: Maybe Integer} deriving (Show)

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

instance FromJSON Chat where
 parseJSON = withObject "chat" $ \o -> do
   chatId                      <- o.: "id"
   type'                       <- o.: "type"
   title                       <- o.:? "title"
   username                    <- o.:? "username"
   first_name                  <- o.:? "first_name"
   last_name                   <- o.:? "last_name"
   return Chat {..}

instance FromJSON Message where
 parseJSON = withObject "message" $ \o -> do
   message_id                 <- o.: "message_id"
   from                       <- o.:? "from"
   date                       <- o.: "date"
   chat                       <- o.: "chat"
   text                       <- o.:? "text"
   sticker                    <- o.:? "sticker"
   return Message {..}

instance FromJSON Results where
 parseJSON = withObject "result" $ \o -> do
   update_id                 <- o.: "update_id"
   message                   <- o.: "message"
   return Results {..}

instance FromJSON Updates where
 parseJSON = withObject "updates" $ \o -> do
   ok                         <- o.:  "ok"
   results                    <- o.:  "result"
   return Updates {..}

instance FromJSON Sticker where
 parseJSON = withObject "sticker" $ \o -> do
   file_id                      <- o.: "file_id"
   file_unique_id               <- o.: "file_unique_id"
   width                        <- o.: "width"
   height                       <- o.: "height"
   is_animated                  <- o.: "is_animated"
   emoji                        <- o.:? "emoji"
   set_name                     <- o.:? "set_name"
   file_size                    <- o.:? "file_size"
   return Sticker {..}

updatesDownload :: IO BSL.ByteString
updatesDownload = simpleHttp address

updates :: IO (Maybe Updates)
updates  = (\x -> decode x :: Maybe Updates) <$> updatesDownload

getUpdates :: IO (Updates)
getUpdates = do
    download <- simpleHttp address
    case decode download :: Maybe Updates of
                   Just x  -> return x
                   Nothing -> return $ Updates { ok = False, results = []}

makeTextMessage :: IO (Text)
makeTextMessage = do
    receivedUpdates <- getUpdates
    messageText     <- pure (fromMaybe (pack []) (text.message.last.results $ receivedUpdates)) :: IO Text
    messageChatId   <- pure (chatId.chat.message.last.results $ receivedUpdates) :: IO Integer
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendMessage?chat_id=", pack $ show messageChatId, pack "&text=", messageText]) :: IO Text
    check           <- simpleHttp (unpack sendingQuery)
    return sendingQuery

makeStickerMessage :: IO (Text)
makeStickerMessage = do
    receivedUpdates <- getUpdates
    sticker_id      <- pure (file_id $ fromJust (sticker.message.last.results $ receivedUpdates)) :: IO Text
    messageChatId   <- pure (chatId.chat.message.last.results $ receivedUpdates) :: IO Integer
    sendingQuery    <- pure (T.concat [pack "https://api.telegram.org/bot1293826122:AAHMwYErxB-irpptkb7tvz8oP8ehHEEzRh8/sendSticker?chat_id=", pack $ show messageChatId, pack "&sticker=", sticker_id]) :: IO Text
    check           <- simpleHttp (unpack sendingQuery)
    return sendingQuery
