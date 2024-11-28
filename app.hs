-- App.hs
module Main where

import qualified Legal.Compliance as Compliance
import qualified Legal.Validator as Validator
import qualified Legal.Rule as Rule
import qualified Legal.Log as Log
import qualified Legal.State as State
import Legal.API (getComplianceStatus)

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- | Main entry point of the application
main :: IO ()
main = do
    -- Get the arguments passed to the program (file paths of legal documents)
    args <- getArgs
    if null args then
        -- No file path provided, show usage instructions
        printUsage
    else do
        let filePath = head args
        
        -- Check if the file exists
        fileExists <- doesFileExist filePath
        if not fileExists then
            -- If the file does not exist, display an error message
            hPutStrLn stderr $ "Error: File '" ++ filePath ++ "' does not exist!"
        else do
            -- If the file exists, start processing it
            putStrLn $ "Processing legal document: " ++ filePath
            
            -- Step 1: Load the document content (simulate loading the document)
            documentContent <- loadDocument filePath

            -- Step 2: Validate the document format and content
            validationResult <- Validator.validateDocument documentContent
            case validationResult of
                Left validationError -> do
                    -- If validation fails, log the error
                    Log.logError validationError
                    hPutStrLn stderr $ "Validation failed: " ++ validationError
                Right validDocument -> do
                    -- Step 3: Check compliance with state rules
                    let complianceStatus = Compliance.checkCompliance validDocument
                    -- Log the compliance check result
                    Log.logComplianceResult complianceStatus

                    -- Step 4: Print the compliance status
                    putStrLn $ "Compliance Status: " ++ show complianceStatus

                    -- Optionally, trigger additional actions, e.g., notifications
                    if complianceStatus == Compliance.Compliant
                        then putStrLn "This document is compliant!"
                        else putStrLn "This document is not compliant."

                    -- Step 5: API Call (if needed)
                    apiResult <- getComplianceStatus filePath
                    putStrLn $ "API Compliance Status: " ++ show apiResult

-- | Function to load document (simulated for the purpose of this example)
loadDocument :: FilePath -> IO String
loadDocument filePath = do
    -- Here we would read the content of the legal document from the file system.
    -- In this case, we're just simulating loading the file content.
    putStrLn $ "Loading document from: " ++ filePath
    return "Simulated document content"

-- | Function to print the usage instructions
printUsage :: IO ()
printUsage = do
    putStrLn "Usage: legal-compliance-checker <document-file-path>"
    putStrLn "Example: legal-compliance-checker documents/legal-document1.txt"
