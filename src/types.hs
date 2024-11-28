module Types where

import Data.Time.Clock (UTCTime)

-- | Defines the possible compliance statuses.
data ComplianceStatus
    = Compliant
    | NonCompliant
    | Unknown
    deriving (Eq, Show)

-- | Represents a legal rule with an identifier, description, and condition.
data Rule = Rule
    { ruleId :: String           -- Unique identifier for the rule
    , description :: String      -- Human-readable description of the rule
    , condition :: [(String, String)] -> Bool -- Function to evaluate the rule
    } deriving (Show)

-- | Represents a log entry.
data LogEntry = LogEntry
    { timestamp :: UTCTime       -- Time of the log entry
    , logLevel :: LogLevel       -- Level of the log (INFO, ERROR, etc.)
    , message :: String          -- Log message
    } deriving (Show)

-- | Log levels for the application.
data LogLevel
    = Info
    | Warning
    | Error
    deriving (Eq, Show)

-- | Represents a notification type.
data NotificationType
    = Email
    | SMS
    | Push
    deriving (Eq, Show)

-- | Represents a notification.
data Notification = Notification
    { notifId :: String          -- Unique notification ID
    , notifType :: NotificationType -- Type of notification
    , content :: String          -- Notification content
    , recipient :: String        -- Recipient (email, phone, etc.)
    } deriving (Show)

-- | Represents a state entity with rules and metadata.
data State = State
    { stateName :: String        -- Name of the state
    , rules :: [Rule]            -- List of rules applicable in the state
    , lastUpdated :: Maybe UTCTime -- Last updated timestamp
    } deriving (Show)

-- | Represents the result of evaluating a rule.
data RuleEvaluationResult = RuleEvaluationResult
    { evaluatedRule :: Rule      -- The rule being evaluated
    , result :: Bool             -- Evaluation result (True for compliance)
    , details :: String          -- Additional details about the result
    } deriving (Show)

-- | Represents an error that can occur during rule evaluation or other operations.
data AppError
    = RuleNotFoundError String   -- Rule ID not found
    | InvalidInputError String   -- Invalid input for a rule or operation
    | SystemError String         -- General system error
    deriving (Show)

-- | Defines the configuration for the application.
data AppConfig = AppConfig
    { databasePath :: FilePath   -- Path to the database or storage
    , logFilePath :: FilePath    -- Path to the log file
    , enableNotifications :: Bool -- Whether notifications are enabled
    } deriving (Show)

-- | Represents a cache structure for state data.
type StateCache = [(String, State)]

-- | Represents the user input for rule evaluation context.
type EvaluationContext = [(String, String)]

-- | Result of compliance evaluation at the state level.
data StateComplianceResult = StateComplianceResult
    { state :: String            -- State name
    , complianceResults :: [RuleEvaluationResult] -- Results for each rule
    , overallStatus :: ComplianceStatus -- Overall compliance status
    } deriving (Show)

-- | Notification priority levels.
data NotificationPriority
    = High
    | Medium
    | Low
    deriving (Eq, Show)

-- | Represents a notification queue item.
data NotificationQueueItem = NotificationQueueItem
    { queueItemId :: String          -- Unique ID for the queue item
    , notification :: Notification   -- Notification details
    , priority :: NotificationPriority -- Priority of the notification
    , scheduledTime :: UTCTime       -- When the notification should be sent
    } deriving (Show)

-- | Represents a hierarchical rule structure (for nested rules).
data HierarchicalRule = HierarchicalRule
    { parentRule :: Rule            -- The parent rule
    , subRules :: [Rule]            -- Sub-rules dependent on the parent
    } deriving (Show)

-- | Represents a result of hierarchical rule evaluation.
data HierarchicalRuleResult = HierarchicalRuleResult
    { parentResult :: Bool          -- Result of the parent rule
    , subRuleResults :: [Bool]      -- Results of the sub-rules
    } deriving (Show)
