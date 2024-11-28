module Validator where

import qualified Rule
import qualified LegalDocument

validateRule :: LegalDocument.Document -> Rule.Rule -> Bool
validateRule doc rule = 
    -- Placeholder for rule validation logic
    "compliance" `elem` words (LegalDocument.content doc)
