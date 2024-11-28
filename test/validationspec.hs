module ValidatorSpec where

import Test.HUnit
import Validator
import Types
import Data.Either (isLeft, isRight)

-- | Test Validating a Single Rule with Correct Input
testValidateSingleRuleSuccess :: Test
testValidateSingleRuleSuccess = TestCase $ do
    let rule = Rule "R101" "Tax Compliance" [("tax", "paid")]
        inputs = [("tax", "paid")]
    let result = validateRule rule inputs
    assertBool "Validation of a correct rule failed" (result == Right True)

-- | Test Validating a Single Rule with Incorrect Input
testValidateSingleRuleFailure :: Test
testValidateSingleRuleFailure = TestCase $ do
    let rule = Rule "R102" "Emission Standards" [("emission", "below_limit")]
        inputs = [("emission", "above_limit")]
    let result = validateRule rule inputs
    assertBool "Validation of an incorrect rule should fail" (result == Right False)

-- | Test Validating Multiple Rules with All Satisfied
testValidateMultipleRulesAllSuccess :: Test
testValidateMultipleRulesAllSuccess = TestCase $ do
    let rules = [ Rule "R201" "Safety Standards" [("safety_check", "pass")]
                , Rule "R202" "Documentation Check" [("docs", "complete")]
                ]
        inputs = [("safety_check", "pass"), ("docs", "complete")]
    let result = validateRules rules inputs
    assertBool "Validation of multiple correct rules failed" (result == Right True)

-- | Test Validating Multiple Rules with Some Failing
testValidateMultipleRulesSomeFail :: Test
testValidateMultipleRulesSomeFail = TestCase $ do
    let rules = [ Rule "R301" "Fire Safety Check" [("fire_safety", "pass")]
                , Rule "R302" "Water Safety Check" [("water_safety", "safe")]
                ]
        inputs = [("fire_safety", "pass"), ("water_safety", "unsafe")]
    let result = validateRules rules inputs
    assertBool "Validation should fail when some rules are incorrect" (result == Right False)

-- | Test Validating Rules with Missing Inputs
testValidateRulesMissingInputs :: Test
testValidateRulesMissingInputs = TestCase $ do
    let rule = Rule "R401" "Waste Management" [("waste_disposal", "approved")]
        inputs = [("tax", "paid")] -- Irrelevant input
    let result = validateRule rule inputs
    assertBool "Validation should fail when required inputs are missing" (result == Left "Missing input for rule: waste_disposal")

-- | Test Validation with Complex Conditions
testValidateComplexRules :: Test
testValidateComplexRules = TestCase $ do
    let rules = [ Rule "R501" "Energy Efficiency" [("energy", "efficient")]
                , Rule "R502" "Renewable Sources" [("renewable", "yes")]
                ]
        inputs = [("energy", "efficient"), ("renewable", "no")]
    let result = validateRules rules inputs
    assertBool "Complex validation failed" (result == Right False)

-- | Test Validation with Empty Rule Set
testValidateEmptyRules :: Test
testValidateEmptyRules = TestCase $ do
    let rules = []
        inputs = [("any_input", "any_value")]
    let result = validateRules rules inputs
    assertBool "Validation of an empty rule set should succeed" (result == Right True)

-- | Test Validation with Invalid Rule Format
testValidateInvalidRule :: Test
testValidateInvalidRule = TestCase $ do
    let invalidRule = Rule "R601" "" [] -- Empty description and conditions
        inputs = [("random", "value")]
    let result = validateRule invalidRule inputs
    assertBool "Validation of an invalid rule should fail" (isLeft result)

-- | Aggregate All Validator Tests
validatorTests :: Test
validatorTests = TestList
    [ TestLabel "Single Rule Validation Success Test" testValidateSingleRuleSuccess
    , TestLabel "Single Rule Validation Failure Test" testValidateSingleRuleFailure
    , TestLabel "Multiple Rules All Success Test" testValidateMultipleRulesAllSuccess
    , TestLabel "Multiple Rules Some Fail Test" testValidateMultipleRulesSomeFail
    , TestLabel "Missing Inputs Test" testValidateRulesMissingInputs
    , TestLabel "Complex Rules Test" testValidateComplexRules
    , TestLabel "Empty Rules Validation Test" testValidateEmptyRules
    , TestLabel "Invalid Rule Format Test" testValidateInvalidRule
    ]

-- | Main Test Runner
main :: IO Counts
main = do
    putStrLn "Running Validator Tests..."
    runTestTT validatorTests
