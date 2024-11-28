module UserInterface where

import qualified Database
import qualified Compliance
import qualified LegalDocument
import qualified Log
import Control.Monad (when)

-- Main entry point for the user interface
main :: IO ()
main = do
    Log.initializeLogging  -- Initialize logging
    putStrLn "Welcome to the State Legal Compliance Checker!"
    userLoop

-- Loop to continuously interact with the user
userLoop :: IO ()
userLoop = do
    putStrLn "\nPlease choose an option:"
    putStrLn "1. Check document compliance"
    putStrLn "2. Exit"
    option <- getLine
    
    case option of
        "1" -> checkDocumentCompliance
        "2" -> exitApplication
        _   -> do
            putStrLn "Invalid option. Please try again."
            userLoop

-- Function to check document compliance
checkDocumentCompliance :: IO ()
checkDocumentCompliance = do
    putStrLn "Please enter the path to the legal document:"
    path <- getLine
    document <- readDocument path  -- Read document from the provided path
    conn <- Database.connect  -- Connect to the database
    
    complianceResult <- Compliance.checkCompliance conn document
    Log.logComplianceResult complianceResult  -- Log the compliance results
    displayComplianceResults complianceResult  -- Display the results to the user
    
    userLoop  -- Go back to the main menu

-- Function to read a legal document from the specified path
readDocument :: FilePath -> IO LegalDocument.Document
readDocument path = do
    contents <- readFile path
    let doc = LegalDocument.createDocument contents path  -- Create a document structure
    Log.logInfo $ "Document loaded: " ++ LegalDocument.title doc
    return doc

-- Display compliance results to the user
displayComplianceResults :: Compliance.ComplianceResult -> IO ()
displayComplianceResults result = do
    putStrLn $ "Document Title: " ++ LegalDocument.title (Compliance.document result)
    putStrLn $ "Compliance Status: " ++ show (Compliance.isCompliant result)
    when (not (Compliance.isCompliant result)) $ do
        putStrLn "Violations:"
        mapM_ putStrLn (Compliance.violations result)

-- Exit the application
exitApplication :: IO ()
exitApplication = do
    Log.logInfo "Application exited by user."
    putStrLn "Thank you for using the State Legal Compliance Checker. Goodbye!"
