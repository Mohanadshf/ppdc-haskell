{-# LANGUAGE TemplateHaskell #-}

module Assignment4Test where

import Data.Char
import Data.List
import Test.QuickCheck hiding (Success, Failure)

import Mooc.Th
import Mooc.Test

import Assignment4

main = score tests

tests = [
    (1, "isMinLength", [ex_isMinLength_success, ex_isMinLength_fail, ex_isMinLength_empty, ex_isMinLength_boundary, ex_isMinLength_zero]),
    (2, "isMaxLength", [ex_isMaxLength_success, ex_isMaxLength_fail, ex_isMaxLength_empty, ex_isMaxLength_boundary, ex_isMaxLength_below]),
    (3, "isExactLength", [ex_isExactLength_success, ex_isExactLength_fail, ex_isExactLength_empty, ex_isExactLength_boundary, ex_isExactLength_tooLong]),
    (4, "isNumeric", [ex_isNumeric_success, ex_isNumeric_fail, ex_isNumeric_empty, ex_isNumeric_spaces, ex_isNumeric_symbols, ex_isNumeric_leadingZeros, ex_isNumeric_mixed]),
    (5, "isAlphabetic", [ex_isAlphabetic_success, ex_isAlphabetic_fail, ex_isAlphabetic_empty, ex_isAlphabetic_spaces, ex_isAlphabetic_symbols, ex_isAlphabetic_upper, ex_isAlphabetic_mixedcase]),
    (6, "isAlphanumeric", [ex_isAlphanumeric_success, ex_isAlphanumeric_fail, ex_isAlphanumeric_empty, ex_isAlphanumeric_letters, ex_isAlphanumeric_digits, ex_isAlphanumeric_spaces, ex_isAlphanumeric_symbols]),
    (7, "isEmail", [ex_isEmail_success, ex_isEmail_fail, ex_isEmail_empty, ex_isEmail_multipleAt, ex_isEmail_atStart, ex_isEmail_atEnd, ex_isEmail_noDotAfterAt]),
    (8, "isInRange", [ex_isInRange_success, ex_isInRange_fail, ex_isInRange_below, ex_isInRange_atMin, ex_isInRange_atMax, ex_isInRange_minGreaterThanMax]),
    (9, "mapError", [ex_mapError_success]),
    (10, "withContext", [ex_withContext_success]),
    (11, "optional", [ex_optional_success, ex_optional_fail]),
    (12, "withDefault", [ex_withDefault_success, ex_withDefault_fail]),
    (13, "mapSuccess", [ex_mapSuccess_success]),
    (14, "andThen", [ex_andThen_success, ex_andThen_fail]),
    (15, "chainValidations", [ex_chainValidations_success, ex_chainValidations_fail]),
    (16, "allOf", [ex_allOf_success, ex_allOf_fail]),
    (17, "anyOf", [ex_anyOf_success, ex_anyOf_fail]),
    (18, "both", [ex_both_success, ex_both_fail]),
    (19, "validateEach", [ex_validateEach_success, ex_validateEach_fail]),
    (20, "validateAtLeast", [ex_validateAtLeast_success, ex_validateAtLeast_fail])
    ]

-- Basic Validation Primitives Tests
ex_isMinLength_success = $(testing [|isMinLength 5 "hello"|]) (?== Success "hello")
ex_isMinLength_fail = $(testing [|isMinLength 3 "hi"|]) (?== Failure ["Value must be at least 3 characters"])

ex_isMaxLength_success = $(testing [|isMaxLength 3 "hi"|]) (?== Success "hi")
ex_isMaxLength_fail = $(testing [|isMaxLength 3 "hello"|]) (?== Failure ["Value must be at most 3 characters"])

ex_isExactLength_success = $(testing [|isExactLength 4 "test"|]) (?== Success "test")
ex_isExactLength_fail = $(testing [|isExactLength 4 "no"|]) (?== Failure ["Value must be exactly 4 characters"])

ex_isNumeric_success = $(testing [|isNumeric "123"|]) (?== Success "123")
ex_isNumeric_fail = $(testing [|isNumeric "abc"|]) (?== Failure ["Value must be numeric"])

ex_isAlphabetic_success = $(testing [|isAlphabetic "hello"|]) (?== Success "hello")
ex_isAlphabetic_fail = $(testing [|isAlphabetic "hello123"|]) (?== Failure ["Value must contain only letters"])

ex_isAlphanumeric_success = $(testing [|isAlphanumeric "hello123"|]) (?== Success "hello123")
ex_isAlphanumeric_fail = $(testing [|isAlphanumeric "hello!"|]) (?== Failure ["Value must be alphanumeric"])

ex_isEmail_success = $(testing [|isEmail "test@example.com"|]) (?== Success "test@example.com")
ex_isEmail_fail = $(testing [|isEmail "invalid.email"|]) (?== Failure ["Value must be a valid email address"])

ex_isInRange_success = $(testing [|isInRange 1 10 5|]) (?== Success 5)
ex_isInRange_fail = $(testing [|isInRange 1 10 11|]) (?== Failure ["Value must be between 1 and 10"])

-- Additional Edge Case and Tricky Input Tests

-- isMinLength edge cases
ex_isMinLength_empty = $(testing [|isMinLength 1 ""|]) (?== Failure ["Value must be at least 1 characters"])
ex_isMinLength_boundary = $(testing [|isMinLength 3 "abc"|]) (?== Success "abc")
ex_isMinLength_zero = $(testing [|isMinLength 0 ""|]) (?== Success "")

-- isMaxLength edge cases
ex_isMaxLength_empty = $(testing [|isMaxLength 0 ""|]) (?== Success "")
ex_isMaxLength_boundary = $(testing [|isMaxLength 2 "ab"|]) (?== Success "ab")
ex_isMaxLength_below = $(testing [|isMaxLength 2 "abc"|]) (?== Failure ["Value must be at most 2 characters"])

-- isExactLength edge cases
ex_isExactLength_empty = $(testing [|isExactLength 0 ""|]) (?== Success "")
ex_isExactLength_boundary = $(testing [|isExactLength 3 "abc"|]) (?== Success "abc")
ex_isExactLength_tooLong = $(testing [|isExactLength 2 "abc"|]) (?== Failure ["Value must be exactly 2 characters"])

-- isNumeric edge cases
ex_isNumeric_empty = $(testing [|isNumeric ""|]) (?== Failure ["Value must be numeric"])
ex_isNumeric_spaces = $(testing [|isNumeric "12 3"|]) (?== Failure ["Value must be numeric"])
ex_isNumeric_symbols = $(testing [|isNumeric "12-3"|]) (?== Failure ["Value must be numeric"])
ex_isNumeric_leadingZeros = $(testing [|isNumeric "00123"|]) (?== Success "00123")
ex_isNumeric_mixed = $(testing [|isNumeric "123abc"|]) (?== Failure ["Value must be numeric"])

-- isAlphabetic edge cases
ex_isAlphabetic_empty = $(testing [|isAlphabetic ""|]) (?== Failure ["Value must contain only letters"])
ex_isAlphabetic_spaces = $(testing [|isAlphabetic "hello world"|]) (?== Failure ["Value must contain only letters"])
ex_isAlphabetic_symbols = $(testing [|isAlphabetic "hello!"|]) (?== Failure ["Value must contain only letters"])
ex_isAlphabetic_upper = $(testing [|isAlphabetic "HELLO"|]) (?== Success "HELLO")
ex_isAlphabetic_mixedcase = $(testing [|isAlphabetic "Hello"|]) (?== Success "Hello")

-- isAlphanumeric edge cases
ex_isAlphanumeric_empty = $(testing [|isAlphanumeric ""|]) (?== Failure ["Value must be alphanumeric"])
ex_isAlphanumeric_letters = $(testing [|isAlphanumeric "abc"|]) (?== Success "abc")
ex_isAlphanumeric_digits = $(testing [|isAlphanumeric "123"|]) (?== Success "123")
ex_isAlphanumeric_spaces = $(testing [|isAlphanumeric "abc 123"|]) (?== Failure ["Value must be alphanumeric"])
ex_isAlphanumeric_symbols = $(testing [|isAlphanumeric "abc!"|]) (?== Failure ["Value must be alphanumeric"])

-- isEmail edge cases
ex_isEmail_empty = $(testing [|isEmail ""|]) (?== Failure ["Value must be a valid email address"])
ex_isEmail_multipleAt = $(testing [|isEmail "a@b@c.com"|]) (?== Failure ["Value must be a valid email address"])
ex_isEmail_atStart = $(testing [|isEmail "@example.com"|]) (?== Failure ["Value must be a valid email address"])
ex_isEmail_atEnd = $(testing [|isEmail "test@"|]) (?== Failure ["Value must be a valid email address"])
ex_isEmail_noDotAfterAt = $(testing [|isEmail "test@examplecom"|]) (?== Failure ["Value must be a valid email address"])

-- isInRange edge cases
ex_isInRange_below = $(testing [|isInRange 1 10 0|]) (?== Failure ["Value must be between 1 and 10"])
ex_isInRange_atMin = $(testing [|isInRange 1 10 1|]) (?== Success 1)
ex_isInRange_atMax = $(testing [|isInRange 1 10 10|]) (?== Success 10)
ex_isInRange_minGreaterThanMax = $(testing [|isInRange 10 1 5|]) (?== Failure ["Value must be between 10 and 1"])

-- Validation Transformations Tests
ex_mapError_success = property $ mapError ("Error: " ++) (Failure ["test"] :: ValidationResult String) == (Failure ["Error: test"] :: ValidationResult String)

ex_withContext_success = $(testing [|withContext "Field" (Failure ["is invalid"] :: ValidationResult String)|])
    (?== (Failure ["Field: is invalid"] :: ValidationResult String))

ex_optional_success = $(testing [|optional (Success "ok")|]) 
    (?== (Success (Just "ok") :: ValidationResult (Maybe String)))
ex_optional_fail = $(testing [|optional (Failure ["error"] :: ValidationResult String)|]) 
    (?== (Success Nothing :: ValidationResult (Maybe String)))

ex_withDefault_success = $(testing [|withDefault "default" (Success "ok")|]) 
    (?== (Success "ok" :: ValidationResult String))
ex_withDefault_fail = $(testing [|withDefault "default" (Failure ["error"] :: ValidationResult String)|]) 
    (?== (Success "default" :: ValidationResult String))

ex_mapSuccess_success = property $ mapSuccess length (Success "hello") == (Success 5 :: ValidationResult Int)

-- Sequential Composition Tests
ex_andThen_success = property $ (Success "123" `andThen` isNumeric) == (Success "123" :: ValidationResult String)
ex_andThen_fail = property $ (Success "abc" `andThen` isNumeric) == (Failure ["Value must be numeric"] :: ValidationResult String)

ex_chainValidations_success = property $ chainValidations [isNotEmpty, isMinLength 3] "hello" == (Success "hello" :: ValidationResult String)
ex_chainValidations_fail = property $ chainValidations [isNotEmpty, isMinLength 3] "hi" == (Failure ["Value must be at least 3 characters"] :: ValidationResult String)

ex_allOf_success = $(testing [|allOf [Success 1, Success 2, Success 3]|]) 
    (?== (Success [1,2,3] :: ValidationResult [Int]))
ex_allOf_fail = $(testing [|allOf [Success 1, Failure ["error"], Success 3]|]) 
    (?== (Failure ["error"] :: ValidationResult [Int]))

ex_anyOf_success = $(testing [|anyOf [Failure ["error1"], Success "good", Failure ["error2"]]|]) 
    (?== (Success "good" :: ValidationResult String))
ex_anyOf_fail = $(testing [|anyOf [Failure ["error1"] :: ValidationResult String, Failure ["error2"] :: ValidationResult String]|]) 
    (?== (Failure ["error1","error2"] :: ValidationResult String))

ex_both_success = $(testing [|both (Success "a") (Success "b")|]) 
    (?== (Success ("a","b") :: ValidationResult (String, String)))
ex_both_fail = $(testing [|both (Failure ["err1"] :: ValidationResult String) (Failure ["err2"] :: ValidationResult String)|]) 
    (?== (Failure ["err1","err2"] :: ValidationResult (String, String)))

ex_validateEach_success = property $ validateEach isNumeric ["123", "456"] == (Success ["123","456"] :: ValidationResult [String])
ex_validateEach_fail = property $ validateEach isNumeric ["123", "abc"] == (Failure ["Value must be numeric"] :: ValidationResult [String])

ex_validateAtLeast_success = $(testing [|validateAtLeast 2 [Success 1, Success 2, Failure ["error"]]|]) (?== Success [1,2])
ex_validateAtLeast_fail = $(testing [|validateAtLeast 2 [Success 1, Failure ["error1"], Failure ["error2"]]|]) 
    (?== Failure ["Not enough valid values: need 2 but got 1"])