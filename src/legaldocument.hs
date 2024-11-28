module LegalDocument where

data Document = Document
    { title       :: String
    , content     :: String
    , author      :: String
    , createdDate :: String
    } deriving (Show)

loadDocument :: FilePath -> IO Document
loadDocument path = do
    content <- readFile path
    -- For simplicity, assuming title is the first line
    let title = head (lines content)
    let author = "Unknown"  -- Placeholder for author extraction logic
    let createdDate = "2024-01-01"  -- Placeholder for date extraction
    return $ Document title content author createdDate
