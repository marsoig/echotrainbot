module ConfigParser where

import Data.Char
import Data.List
import Data.List.Split

data Config = Config
    { telegramToken :: String
    , logLevel :: String
    } deriving (Show)

parseConfig:: String -> Config
parseConfig s = foldr addValue defaultConfig (getConfig s)

getConfig :: String -> [(String,String)]
getConfig = clean.map (span (/= ' ')).filter (\x -> head x /= '#').lines

clean :: [(String, String)] -> [(String, String)]
clean = map (\(x,y) -> (x, drop 3 y))

mainTest :: IO ()
mainTest = readFile "config.txt" >>= (print . parseConfig)

addValue :: (String, String) -> Config -> Config
addValue (key, value) config = case key of
  "telegramToken" -> config {telegramToken = value}
  "logLevel"      -> config {logLevel      = value}
  _               -> config

defaultConfig :: Config
defaultConfig = Config "" ""
