module API where

import Types
import Compliance (evaluateStateCompliance, evaluateRule)
import Notifications (sendNotification)
import Log (logMessage)
import State (getStateByName, updateStateRules, listAllStates)
import Data.Time.Clock (getCurrentTime)

-- | Fetch compliance data for a given state.
getComplianceData :: String -> EvaluationContext -> IO (Either AppError StateComplianceResult)
getComplianceData stateName context = do
    -- Fetch state data
    maybeState <- getStateByName stateName
    case maybeState of
        Nothing -> return $ Left $ RuleNotFoundError $ "State not found: " ++ stateName
        Just state -> do
            -- Evaluate compliance for the state
            complianceResult <- evaluateStateCompliance state context
            return $ Right complianceResult

-- | Evaluate a specific rule by ID for a given context.
evaluateRuleById :: String -> EvaluationContext -> IO (Either AppError RuleEvaluationResult)
evaluateRuleById ruleId context = do
    -- Try to fetch the rule from all states
    allStates <- listAllStates
    let allRules = concatMap rules allStates
    case filter (\rule -> ruleId (ruleId rule)) allRules of
        [] -> return $ Left $ RuleNotFoundError $ "Rule not found: " ++ ruleId
        (rule:_) -> do
            -- Evaluate the rule
            let result = evaluateRule rule context
            let details = if result then "Rule passed" else "Rule failed"
            return $ Right $ RuleEvaluationResult rule result details

-- | Send a notification to a user.
sendUserNotification :: Notification -> IO (Either AppError ())
sendUserNotification notif = do
    -- Send the notification
    result <- sendNotification notif
    case result of
        True -> do
            logMessage Info $ "Notification sent: " ++ notifId notif
            return $ Right ()
        False -> do
            logMessage Error $ "Failed to send notification: " ++ notifId notif
            return $ Left $ SystemError "Failed to send notification"

-- | Update state rules programmatically.
updateState :: String -> [Rule] -> IO (Either AppError ())
updateState stateName newRules = do
    -- Attempt to update the state's rules
    result <- updateStateRules stateName newRules
    case result of
        True -> do
            logMessage Info $ "State rules updated for: " ++ stateName
            return $ Right ()
        False -> do
            logMessage Error $ "Failed to update state rules for: " ++ stateName
            return $ Left $ SystemError "Failed to update state rules"

-- | Retrieve a list of all available states.
getAllStates :: IO [String]
getAllStates = do
    states <- listAllStates
    return $ map stateName states

-- | Log an event programmatically.
logEvent :: LogLevel -> String -> IO ()
logEvent level message = do
    currentTime <- getCurrentTime
    let logEntry = LogEntry currentTime level message
    logMessage level message

-- | Fetch details of a specific state.
getStateDetails :: String -> IO (Either AppError State)
getStateDetails stateName = do
    maybeState <- getStateByName stateName
    case maybeState of
        Nothing -> return $ Left $ RuleNotFoundError $ "State not found: " ++ stateName
        Just state -> return $ Right state

-- | Bulk evaluate compliance for multiple states.
bulkEvaluateCompliance :: [String] -> EvaluationContext -> IO [(String, Either AppError StateComplianceResult)]
bulkEvaluateCompliance stateNames context = mapM (\stateName -> do
    compliance <- getComplianceData stateName context
    return (stateName, compliance)) stateNames

-- | Schedule a notification for the future.
scheduleNotification :: NotificationQueueItem -> IO (Either AppError ())
scheduleNotification queueItem = do
    currentTime <- getCurrentTime
    if scheduledTime queueItem < currentTime
        then return $ Left $ InvalidInputError "Scheduled time cannot be in the past"
        else do
            logMessage Info $ "Notification scheduled: " ++ queueItemId queueItem
            return $ Right ()

-- | Retrieve compliance results for a state.
getStateComplianceResults :: String -> IO (Either AppError [RuleEvaluationResult])
getStateComplianceResults stateName = do
    complianceResult <- getComplianceData stateName []
    case complianceResult of
        Left err -> return $ Left err
        Right result -> return $ Right $ complianceResults result

-- | Add a new rule to a state's rule set.
addRuleToState :: String -> Rule -> IO (Either AppError ())
addRuleToState stateName newRule = do
    stateResult <- getStateByName stateName
    case stateResult of
        Nothing -> return $ Left $ RuleNotFoundError $ "State not found: " ++ stateName
        Just state -> do
            let updatedRules = newRule : rules state
            updateResult <- updateStateRules stateName updatedRules
            if updateResult
                then do
                    logMessage Info $ "Rule added to state: " ++ stateName
                    return $ Right ()
                else do
                    logMessage Error $ "Failed to add rule to state: " ++ stateName
                    return $ Left $ SystemError "Failed to update rules"

