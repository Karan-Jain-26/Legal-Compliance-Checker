module Compliance where

import qualified Database
import qualified Rule
import qualified LegalDocument
import qualified Validator
import qualified Log
import Control.Monad (forM, forM_)
import Control.Exception (try, SomeException)

-- Data structure to hold the compliance result with detailed information
data ComplianceResult = ComplianceResult
    { document :: LegalDocument.Document
    , isCompliant :: Bool
    , violations :: [String]
    } deriving (Show)

-- Check compliance for a given document
checkCompliance :: Database.Connection -> LegalDocument.Document -> IO ComplianceResult
checkCompliance conn doc = do
    rules <- Rule.loadRules conn
    Log.logInfo $ "Checking compliance for document: " ++ LegalDocument.title doc
    
    -- Validate each rule and gather results
    results <- forM rules $ \rule -> do
        result <- try (evaluateRule doc rule) :: IO (Either SomeException Bool)
        case result of
            Right compliant -> return (ruleName rule, compliant)
            Left err -> do
                Log.logError $ "Error evaluating rule: " ++ show err
                return (ruleName rule, False)  -- Assume non-compliance on error
    
    let violations = [ruleName | (ruleName, False) <- results]
    let compliant = null violations
    
    Log.logInfo $ "Compliance check completed for: " ++ LegalDocument.title doc ++ ". Compliant: " ++ show compliant
    
    return $ ComplianceResult doc compliant violations

-- Evaluate an individual rule against the document
evaluateRule :: LegalDocument.Document -> Rule.Rule -> IO Bool
evaluateRule doc rule = do
    let isValid = Validator.validateRule doc rule
    Log.logInfo $ "Evaluating rule: " ++ ruleName rule ++ " - Result: " ++ show isValid
    return isValid

-- Log the result of the compliance check
logComplianceResult :: ComplianceResult -> IO ()
logComplianceResult result = do
    let docTitle = LegalDocument.title (document result)
    let compliant = isCompliant result
    let violationsList = violations result
    Log.logInfo $ "Compliance Result for " ++ docTitle ++ ": " ++ show compliant
    unless compliant $ do
        Log.logWarning $ "Violations found: " ++ unwords violationsList

-- Enhanced logging functions
logInfo :: String -> IO ()
logInfo message = Log.logInfo message

logError :: String -> IO ()
logError message = Log.logError message

logWarning :: String -> IO ()
logWarning message = Log.logWarning message
