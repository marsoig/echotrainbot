import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = readFile "config.txt" >>= (print . parseConfig)

parseConfig :: String -> Config
parseConfig = foldr addConfigValue defaultConfig . clean . lines
    where clean = filter (not . flip any ["#", ";", "", " "] . (==) . take 1)

addConfigValue :: String -> Config -> Config
addConfigValue raw config = case key of
    "telegramToken" -> config {telegramToken  = values}
    "logLevel"      -> config {logLevel       = values}
    _               -> config
    where (k, vs) = span (/= ' ') raw
          key = map toLower k
          values = tail vs

data Config = Config
    { telegramToken :: String
    , logLevel :: String
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config "" ""
