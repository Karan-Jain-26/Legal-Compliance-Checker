module Notifications where

import Network.Mail.SMTP
import System.IO
import Log (logInfo, logError)
import Data.Time.Clock
import Data.Time.Format
import System.Environment (getEnv)
import Control.Exception (try, SomeException)

-- Data type for notification channels
data NotificationChannel = Email | SMS | InApp deriving (Show, Eq)

-- Notification data structure
data Notification = Notification
    { recipient :: String
    , subject :: String
    , messageBody :: String
    , channel :: NotificationChannel
    } deriving (Show)

-- Send a notification based on the channel
sendNotification :: Notification -> IO ()
sendNotification notif = do
    case channel notif of
        Email  -> sendEmailNotification notif
        SMS    -> sendSMSNotification notif
        InApp  -> sendInAppNotification notif

-- Send email notification
sendEmailNotification :: Notification -> IO ()
sendEmailNotification notif = do
    let recipientEmail = recipient notif
    let subjectText = subject notif
    let bodyText = messageBody notif

    -- Fetch email credentials from environment variables
    smtpHost <- getEnv "SMTP_HOST"
    smtpUser <- getEnv "SMTP_USER"
    smtpPass <- getEnv "SMTP_PASS"
    
    let fromAddress = Address Nothing smtpUser
    let toAddress = [Address Nothing recipientEmail]
    let mail = simpleMail fromAddress toAddress [] [] subjectText [plainTextPart bodyText]
    
    result <- try $ sendMailWithLogin' smtpHost smtpUser smtpPass mail :: IO (Either SomeException ())
    case result of
        Left ex -> do
            logError $ "Failed to send email: " ++ show ex
        Right _ -> do
            logInfo $ "Email sent successfully to: " ++ recipientEmail

-- Send SMS notification (mock implementation for simplicity)
sendSMSNotification :: Notification -> IO ()
sendSMSNotification notif = do
    let recipientNumber = recipient notif
    let bodyText = messageBody notif
    -- Simulate SMS sending
    logInfo $ "Sending SMS to " ++ recipientNumber ++ ": " ++ bodyText
    putStrLn $ "[SMS SENT]: " ++ bodyText

-- Send in-app notification
sendInAppNotification :: Notification -> IO ()
sendInAppNotification notif = do
    let user = recipient notif
    let alertText = messageBody notif
    logInfo $ "In-app notification sent to user: " ++ user
    putStrLn $ "[In-App Notification] To: " ++ user ++ " - " ++ alertText

-- Generate a timestamped notification message
generateNotificationMessage :: String -> IO String
generateNotificationMessage msg = do
    timestamp <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
    return $ "[" ++ formattedTime ++ "] " ++ msg

-- Send a test notification
sendTestNotifications :: IO ()
sendTestNotifications = do
    -- Test email notification
    let emailNotif = Notification
            { recipient = "user@example.com"
            , subject = "Test Email Notification"
            , messageBody = "This is a test email notification from the system."
            , channel = Email
            }
    sendNotification emailNotif

    -- Test SMS notification
    let smsNotif = Notification
            { recipient = "+1234567890"
            , subject = "Test SMS Notification" -- Subject is ignored for SMS
            , messageBody = "This is a test SMS notification."
            , channel = SMS
            }
    sendNotification smsNotif

    -- Test in-app notification
    let inAppNotif = Notification
            { recipient = "user123"
            , subject = "Test In-App Notification"
            , messageBody = "This is a test in-app notification."
            , channel = InApp
            }
    sendNotification inAppNotif

-- Configure notification system (placeholder)
configureNotifications :: IO ()
configureNotifications = do
    logInfo "Initializing notification system..."
    -- Additional setup can be added here
    return ()
