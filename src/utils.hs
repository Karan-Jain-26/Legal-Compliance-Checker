module Utils
  ( splitOn
  , strip
  , joinWith
  , validateNonEmpty
  , safeHead
  , lookupWithDefault
  , parseConfig
  , formatDate
  , generateUniqueID
  , logMessage
  , convertToKeyValue
  ) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Random (randomIO)

-- | Splits a string by a given delimiter.
splitOn :: Char -> String -> [String]
splitOn delimiter str = case break (== delimiter) str of
  (before, _:after) -> before : splitOn delimiter after
  (before, "") -> [before]

-- | Removes leading and trailing whitespace from a string.
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Joins a list of strings with a given delimiter.
joinWith :: Char -> [String] -> String
joinWith delimiter = intercalate [delimiter]

-- | Validates that a string is not empty, returning either the value or an error message.
validateNonEmpty :: String -> Either String String
validateNonEmpty str
  | null str = Left "Value cannot be empty."
  | otherwise = Right str

-- | Safely retrieves the first element of a list, or returns Nothing if the list is empty.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Looks up a key in an association list, with a default value if the key is not found.
lookupWithDefault :: Eq k => k -> [(k, v)] -> v -> v
lookupWithDefault key list defaultValue = case lookup key list of
  Just value -> value
  Nothing -> defaultValue

-- | Parses a configuration file (key=value format) into an association list.
parseConfig :: String -> [(String, String)]
parseConfig content = map parseLine (lines content)
  where
    parseLine line =
      case splitOn '=' line of
        [key, value] -> (strip key, strip value)
        _ -> error $ "Invalid configuration line: " ++ line

-- | Formats the current date and time as a string.
formatDate :: IO String
formatDate = do
  currentTime <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime

-- | Generates a unique identifier as a string.
generateUniqueID :: IO String
generateUniqueID = do
  randomNum <- randomIO :: IO Int
  return $ "ID-" ++ show (abs randomNum)

-- | Logs a message to the console with a timestamp.
logMessage :: String -> IO ()
logMessage message = do
  timestamp <- formatDate
  putStrLn $ "[" ++ timestamp ++ "] " ++ message

-- | Converts a list of key-value pairs into a formatted string.
convertToKeyValue :: [(String, String)] -> String
convertToKeyValue = intercalate ", " . map (\(key, value) -> key ++ "=" ++ value)

