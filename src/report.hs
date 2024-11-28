module Report where

import qualified LegalDocument

generateReport :: LegalDocument.Document -> Bool -> String
generateReport doc compliant =
    let status = if compliant then "Compliant" else "Non-Compliant"
    in "Report for: " ++ LegalDocument.title doc ++ "\nStatus: " ++ status
