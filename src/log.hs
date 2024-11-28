module Log where

import System.IO
import System.Directory
import Data.Time.Clock
import Data.Time.Format
import Control.Monad (when)
import System.FilePath ((</>))

-- Define log levels
data LogLevel = INFO | WARNING | ERROR deriving (Eq, Ord, Show)

-- File paths for logs
logFile :: FilePath
logFile = "logs/app.log"

rotatedLogDir :: FilePath
rotatedLogDir = "logs/rotated/"

-- Initialize the logging system
initializeLogging :: IO ()
initializeLogging = do
    createDirectoryIfMissing True "logs"
    createDirectoryIfMissing True rotatedLogDir
    rotateLogs logFile 5  -- Rotate if necessary (keep 5 old logs)
    appendFile logFile "" -- Ensure log file exists

-- Write a log message with timestamp and level
writeLog :: LogLevel -> String -> IO ()
writeLog level message = do
    timestamp <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
    let logMessage = "[" ++ formattedTime ++ "] [" ++ show level ++ "] " ++ message ++ "\n"
    appendFile logFile logMessage
    when (level == ERROR) $ putStrLn ("[ERROR]: " ++ message)  -- Print errors to console

-- Convenience functions for logging
logInfo :: String -> IO ()
logInfo = writeLog INFO

logWarning :: String -> IO ()
logWarning = writeLog WARNING

logError :: String -> IO ()
logError = writeLog ERROR

-- Rotate logs to manage file size and keep history
rotateLogs :: FilePath -> Int -> IO ()
rotateLogs currentLog maxOldLogs = do
    exists <- doesFileExist currentLog
    when exists $ do
        size <- getFileSize currentLog
        when (size > maxLogSize) $ do
            timestamp <- getCurrentTime
            let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d_%H%M%S" timestamp
            let rotatedFile = rotatedLogDir </> "app_" ++ formattedTime ++ ".log"
            renameFile currentLog rotatedFile
            cleanOldLogs maxOldLogs

-- Remove old rotated logs exceeding the maximum count
cleanOldLogs :: Int -> IO ()
cleanOldLogs maxOldLogs = do
    files <- listDirectory rotatedLogDir
    let fullPaths = map (rotatedLogDir </>) files
    let sortedPaths = reverse $ take maxOldLogs $ reverse fullPaths
    mapM_ removeFile (fullPaths \\ sortedPaths)

-- Get the size of a file (in bytes)
getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize

-- Configuration: maximum log file size (e.g., 1MB)
maxLogSize :: Integer
maxLogSize = 1024 * 1024
