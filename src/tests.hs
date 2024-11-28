module Tests where

import Test.HUnit
import Types
import API
import Compliance
import Notifications
import State
import Log

-- | Test State Compliance Evaluation
testStateCompliance :: Test
testStateCompliance = TestCase $ do
    let mockState = State "California" [Rule "R1" "Taxes Paid" [("tax", "paid")], Rule "R2" "Insurance Valid" [("insurance", "valid")]]
        mockContext = [("tax", "paid"), ("insurance", "valid")]
    result <- evaluateStateCompliance mockState mockContext
    let expected = StateComplianceResult "California" [RuleEvaluationResult (Rule "R1" "Taxes Paid" [("tax", "paid")]) True "Rule passed", RuleEvaluationResult (Rule "R2" "Insurance Valid" [("insurance", "valid")]) True "Rule passed"]
    assertEqual "State compliance evaluation failed" expected result

-- | Test Rule Evaluation
testRuleEvaluation :: Test
testRuleEvaluation = TestCase $ do
    let rule = Rule "R1" "Taxes Paid" [("tax", "paid")]
        context = [("tax", "paid")]
    let result = evaluateRule rule context
    assertBool "Rule evaluation failed" result

-- | Test Notifications Sending
testNotifications :: Test
testNotifications = TestCase $ do
    let notification = Notification "N1" Email "Compliance Updated!" "user@example.com"
    result <- sendUserNotification notification
    assertEqual "Notification sending failed" (Right ()) result

-- | Test Logging
testLogging :: Test
testLogging = TestCase $ do
    logEvent Info "This is a test log message"
    logEvent Error "This is an error log message"
    -- Assume logging doesn't fail; ensure it executes without exceptions.
    assertBool "Logging failed unexpectedly" True

-- | Test State Fetching
testFetchState :: Test
testFetchState = TestCase $ do
    maybeState <- getStateByName "California"
    case maybeState of
        Nothing -> assertFailure "State not found"
        Just state -> assertEqual "Fetched state mismatch" "California" (stateName state)

-- | Test Bulk Compliance Evaluation
testBulkCompliance :: Test
testBulkCompliance = TestCase $ do
    let mockStates = ["California", "Texas"]
        mockContext = [("tax", "paid"), ("insurance", "valid")]
    results <- bulkEvaluateCompliance mockStates mockContext
    assertEqual "Bulk compliance evaluation failed" 2 (length results)

-- | Test Adding Rules to State
testAddRule :: Test
testAddRule = TestCase $ do
    let newRule = Rule "R3" "New Rule" [("newKey", "newValue")]
    result <- addRuleToState "California" newRule
    case result of
        Left err -> assertFailure $ "Adding rule failed: " ++ show err
        Right _ -> return ()

-- | Test Update State Rules
testUpdateStateRules :: Test
testUpdateStateRules = TestCase $ do
    let newRules = [Rule "R1" "Updated Rule" [("updatedKey", "updatedValue")]]
    result <- updateState "California" newRules
    case result of
        Left err -> assertFailure $ "Updating state rules failed: " ++ show err
        Right _ -> return ()

-- | Test API: Fetch Compliance Data
testAPICompliance :: Test
testAPICompliance = TestCase $ do
    let mockContext = [("tax", "paid"), ("insurance", "valid")]
    result <- getComplianceData "California" mockContext
    case result of
        Left err -> assertFailure $ "API Compliance fetch failed: " ++ show err
        Right complianceResult -> do
            assertEqual "State name mismatch in compliance result" "California" (stateName complianceResult)

-- | Test API: Retrieve All States
testAPIGetAllStates :: Test
testAPIGetAllStates = TestCase $ do
    states <- getAllStates
    assertBool "No states retrieved" (not (null states))

-- | Aggregate All Tests
tests :: Test
tests = TestList
    [ TestLabel "State Compliance Evaluation" testStateCompliance
    , TestLabel "Rule Evaluation" testRuleEvaluation
    , TestLabel "Notifications" testNotifications
    , TestLabel "Logging" testLogging
    , TestLabel "Fetch State" testFetchState
    , TestLabel "Bulk Compliance Evaluation" testBulkCompliance
    , TestLabel "Add Rule" testAddRule
    , TestLabel "Update State Rules" testUpdateStateRules
    , TestLabel "API Compliance Fetch" testAPICompliance
    , TestLabel "API Get All States" testAPIGetAllStates
    ]

-- | Run All Tests
main :: IO Counts
main = do
    putStrLn "Running Tests..."
    runTestTT tests
