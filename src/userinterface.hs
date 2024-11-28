module UserInterface where

import qualified LegalDocument
import qualified Compliance
import qualified Report

startInterface :: IO ()
startInterface = do
    putStrLn "Welcome to the State Legal Compliance Checker!"
    putStrLn "Enter the path to the legal document:"
    path <- getLine
    doc <- LegalDocument.loadDocument path
    compliant <- Compliance.checkCompliance conn doc
    putStrLn $ Report.generateReport doc compliant
