module ComplianceSpec where

import Test.HUnit
import Complianc
import Types
import State
import Utils

-- | Test Compliance Evaluation for a Single State
testEvaluateStateCompliance :: Test
testEvaluateStateCompliance = TestCase $ do
    let rules = [Rule "R1" "Taxes Paid" [("tax", "paid")], Rule "R2" "Insurance Valid" [("insurance", "valid")]]
    let state = State "California" rules
    let context = [("tax", "paid"), ("insurance", "valid")]
    result <- evaluateStateCompliance state context
    let expected = StateComplianceResult "California"
                     [ RuleEvaluationResult (Rule "R1" "Taxes Paid" [("tax", "paid")]) True "Rule passed"
                     , RuleEvaluationResult (Rule "R2" "Insurance Valid" [("insurance", "valid")]) True "Rule passed"
                     ]
    assertEqual "State compliance evaluation failed" expected result

-- | Test Rule Evaluation Logic
testEvaluateRule :: Test
testEvaluateRule = TestCase $ do
    let rule = Rule "R1" "Property Ownership Verified" [("property", "owned")]
    let validContext = [("property", "owned")]
    let invalidContext = [("property", "not_owned")]

    -- Valid rule context
    let result1 = evaluateRule rule validContext
    assertBool "Valid rule failed evaluation" result1

    -- Invalid rule context
    let result2 = evaluateRule rule invalidContext
    assertBool "Invalid rule incorrectly passed" (not result2)

-- | Test Bulk Compliance Evaluation
testBulkEvaluateCompliance :: Test
testBulkEvaluateCompliance = TestCase $ do
    let rules1 = [Rule "R1" "Taxes Paid" [("tax", "paid")], Rule "R2" "Insurance Valid" [("insurance", "valid")]]
    let rules2 = [Rule "R3" "License Renewal" [("license", "renewed")]]
    let states = [State "California" rules1, State "Nevada" rules2]
    let context = [("tax", "paid"), ("insurance", "valid"), ("license", "renewed")]

    results <- bulkEvaluateCompliance (map stateName states) context
    assertEqual "Bulk compliance evaluation failed" 2 (length results)

-- | Test Adding Rules to State Compliance
testAddRuleToStateCompliance :: Test
testAddRuleToStateCompliance = TestCase $ do
    let stateName = "California"
    let newRule = Rule "R3" "New Rule" [("newKey", "newValue")]

    -- Add rule
    result <- addRuleToState stateName newRule
    case result of
        Left err -> assertFailure $ "Adding rule failed: " ++ err
        Right updatedState -> do
            let ruleNames = map ruleName (stateRules updatedState)
            assertBool "New rule not added" ("R3" `elem` ruleNames)

-- | Test Validation of State Rules
testValidateRules :: Test
testValidateRules = TestCase $ do
    let validRules = [Rule "R1" "Taxes Paid" [("tax", "paid")], Rule "R2" "Insurance Valid" [("insurance", "valid")]]
    let invalidRules = [Rule "R3" "Empty Rule" []]

    -- Valid rules
    let result1 = validateRules validRules
    assertBool "Valid rules failed validation" result1

    -- Invalid rules
    let result2 = validateRules invalidRules
    assertBool "Invalid rules incorrectly passed validation" (not result2)

-- | Test Rule Formatting for Logging
testFormatRuleForLog :: Test
testFormatRuleForLog = TestCase $ do
    let rule = Rule "R1" "Taxes Paid" [("tax", "paid"), ("insurance", "valid")]
    let expected = "R1: Taxes Paid (tax=paid, insurance=valid)"
    let result = formatRuleForLog rule
    assertEqual "Rule formatting for log failed" expected result

-- | Aggregate All Compliance Tests
complianceTests :: Test
complianceTests = TestList
    [ TestLabel "Evaluate State Compliance" testEvaluateStateCompliance
    , TestLabel "Evaluate Rule Logic" testEvaluateRule
    , TestLabel "Bulk Compliance Evaluation" testBulkEvaluateCompliance
    , TestLabel "Add Rule to State Compliance" testAddRuleToStateCompliance
    , TestLabel "Validate State Rules" testValidateRules
    , TestLabel "Format Rule for Log" testFormatRuleForLog
    ]

-- | Main Test Runner
main :: IO Counts
main = do
    putStrLn "Running Compliance Tests..."
    runTestTT complianceTests
