module State where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Log (logInfo, logError)
import Rule (Rule, evaluateRule, loadRulesForState)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- State data type to represent compliance rules and metadata
data State = State
    { stateName :: String
    , rules :: [Rule]
    , lastUpdated :: Maybe UTCTime
    } deriving (Show)

-- A cache to store state configurations in memory
type StateCache = Map.Map String State

-- Initialize the cache
initializeStateCache :: IO StateCache
initializeStateCache = do
    logInfo "Initializing state cache..."
    return Map.empty

-- Load state data (from cache or a database/file system)
loadState :: String -> StateCache -> IO (StateCache, Maybe State)
loadState stateName cache = do
    case Map.lookup stateName cache of
        Just state -> do
            logInfo $ "State data for " ++ stateName ++ " loaded from cache."
            return (cache, Just state)
        Nothing -> do
            logInfo $ "Loading state data for " ++ stateName ++ " from external source..."
            rules <- loadRulesForState stateName
            currentTime <- getCurrentTime
            let newState = State
                    { stateName = stateName
                    , rules = rules
                    , lastUpdated = Just currentTime
                    }
            let updatedCache = Map.insert stateName newState cache
            logInfo $ "State data for " ++ stateName ++ " loaded successfully."
            return (updatedCache, Just newState)

-- Evaluate compliance for a given state
evaluateStateCompliance :: State -> [(String, String)] -> IO [(String, Bool)]
evaluateStateCompliance state context = do
    logInfo $ "Evaluating compliance for state: " ++ stateName state
    let results = map (\rule -> (show rule, evaluateRule rule context)) (rules state)
    mapM_ (logResult . fst) results
    return results
  where
    logResult ruleName = logInfo $ "Rule evaluated: " ++ ruleName

-- Add or update state rules
updateStateRules :: String -> [Rule] -> StateCache -> IO StateCache
updateStateRules stateName newRules cache = do
    currentTime <- getCurrentTime
    let updatedState = State
            { stateName = stateName
            , rules = newRules
            , lastUpdated = Just currentTime
            }
    let updatedCache = Map.insert stateName updatedState cache
    logInfo $ "Rules for state " ++ stateName ++ " updated successfully."
    return updatedCache

-- Remove state from cache
removeStateFromCache :: String -> StateCache -> IO StateCache
removeStateFromCache stateName cache = do
    let updatedCache = Map.delete stateName cache
    logInfo $ "State " ++ stateName ++ " removed from cache."
    return updatedCache

-- Get last update timestamp for a state
getLastUpdated :: State -> IO String
getLastUpdated state = do
    let timestamp = fromMaybe "Never" (fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (lastUpdated state))
    return $ "Last updated for " ++ stateName state ++ ": " ++ timestamp

-- List all states in cache
listCachedStates :: StateCache -> IO [String]
listCachedStates cache = do
    let states = Map.keys cache
    logInfo "Listing all states in cache..."
    return states

-- Save state data to a persistent source (e.g., database or file system)
saveStateToStorage :: State -> IO ()
saveStateToStorage state = do
    logInfo $ "Saving state data for " ++ stateName state ++ " to persistent storage..."
    -- Implement actual saving logic here (e.g., writing to a database or file)
    logInfo $ "State data for " ++ stateName state ++ " saved successfully."

-- Example usage: Loading and evaluating compliance for a state
exampleUsage :: IO ()
exampleUsage = do
    cache <- initializeStateCache
    (cache', maybeState) <- loadState "California" cache
    case maybeState of
        Just state -> do
            let context = [("rule1", "value1"), ("rule2", "value2")]
            results <- evaluateStateCompliance state context
            mapM_ (logInfo . show) results
        Nothing -> logError "Failed to load state data."

        
