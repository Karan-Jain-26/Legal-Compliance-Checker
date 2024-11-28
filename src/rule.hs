module Rule where

import Database.PostgreSQL.Simple
import Database

data Rule = Rule
    { ruleId   :: Int
    , ruleName :: String
    , ruleText :: String
    } deriving (Show)

loadRules :: Database.Connection -> IO [Rule]
loadRules (Database.Connection conn) = do
    query conn "SELECT id, name, text FROM rules" ()
