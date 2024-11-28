module FileParser where

import qualified LegalDocument

parseLegalDocument :: FilePath -> IO LegalDocument.Document
parseLegalDocument path = LegalDocument.loadDocument path
