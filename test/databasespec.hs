module DatabaseSpec where

import Test.HUnit
import Database
import Types
import Control.Exception (try, SomeException)
import Data.Maybe (isJust)

-- | Test Database Connection Establishment
testDatabaseConnection :: Test
testDatabaseConnection = TestCase $ do
    connectionResult <- try connectDatabase :: IO (Either SomeException DatabaseConnection)
    case connectionResult of
        Left ex -> assertFailure $ "Failed to establish database connection: " ++ show ex
        Right conn -> do
            isValid <- isConnectionValid conn
            assertBool "Database connection is not valid" isValid
            disconnectDatabase conn

-- | Test Adding a Rule to the Database
testAddRuleToDatabase :: Test
testAddRuleToDatabase = TestCase $ do
    conn <- connectDatabase
    let newRule = Rule "R101" "Environmental Check" [("emission", "below_limit")]
    result <- addRule conn newRule
    assertBool "Failed to add rule to the database" result
    disconnectDatabase conn

-- | Test Retrieving a Rule from the Database
testGetRuleFromDatabase :: Test
testGetRuleFromDatabase = TestCase $ do
    conn <- connectDatabase
    let ruleID = "R101"
    result <- getRuleById conn ruleID
    case result of
        Nothing -> assertFailure "Failed to retrieve rule from the database"
        Just rule -> assertEqual "Retrieved rule does not match"
                       (Rule "R101" "Environmental Check" [("emission", "below_limit")])
                       rule
    disconnectDatabase conn

-- | Test Updating a Rule in the Database
testUpdateRuleInDatabase :: Test
testUpdateRuleInDatabase = TestCase $ do
    conn <- connectDatabase
    let updatedRule = Rule "R101" "Environmental Standards Updated" [("emission", "near_zero")]
    updateResult <- updateRule conn updatedRule
    assertBool "Failed to update rule in the database" updateResult

    -- Verify the update
    retrievedRule <- getRuleById conn "R101"
    case retrievedRule of
        Nothing -> assertFailure "Updated rule not found in the database"
        Just rule -> assertEqual "Updated rule does not match" updatedRule rule
    disconnectDatabase conn

-- | Test Deleting a Rule from the Database
testDeleteRuleFromDatabase :: Test
testDeleteRuleFromDatabase = TestCase $ do
    conn <- connectDatabase
    let ruleID = "R101"
    deleteResult <- deleteRule conn ruleID
    assertBool "Failed to delete rule from the database" deleteResult

    -- Verify deletion
    retrievedRule <- getRuleById conn ruleID
    assertBool "Rule still exists after deletion" (retrievedRule == Nothing)
    disconnectDatabase conn

-- | Test Adding and Retrieving States from the Database
testStateOperations :: Test
testStateOperations = TestCase $ do
    conn <- connectDatabase
    let state = State "California" [Rule "R1" "Taxes Paid" [("tax", "paid")]]
    addStateResult <- addState conn state
    assertBool "Failed to add state to the database" addStateResult

    retrievedState <- getStateByName conn "California"
    case retrievedState of
        Nothing -> assertFailure "Failed to retrieve state from the database"
        Just st -> assertEqual "Retrieved state does not match" state st

    disconnectDatabase conn

-- | Test Bulk Rule Retrieval
testBulkRuleRetrieval :: Test
testBulkRuleRetrieval = TestCase $ do
    conn <- connectDatabase
    rules <- getAllRules conn
    assertBool "Failed to retrieve rules from the database" (not (null rules))
    disconnectDatabase conn

-- | Aggregate All Database Tests
databaseTests :: Test
databaseTests = TestList
    [ TestLabel "Database Connection Test" testDatabaseConnection
    , TestLabel "Add Rule Test" testAddRuleToDatabase
    , TestLabel "Get Rule Test" testGetRuleFromDatabase
    , TestLabel "Update Rule Test" testUpdateRuleInDatabase
    , TestLabel "Delete Rule Test" testDeleteRuleFromDatabase
    , TestLabel "State Operations Test" testStateOperations
    , TestLabel "Bulk Rule Retrieval Test" testBulkRuleRetrieval
    ]

-- | Main Test Runner
main :: IO Counts
main = do
    putStrLn "Running Database Tests..."
    runTestTT databaseTests
