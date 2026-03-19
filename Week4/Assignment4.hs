-- ============================================================================
-- Assignment 4: Validation Combinators
-- ============================================================================


module Assignment4 where

import Data.Char (isDigit, isAlpha, isAlphaNum)

import Mooc.Todo

-- ============================================================================
-- Core Types and Helper Functions
-- ============================================================================

-- Our main validation type (like Curve for graphics)
data ValidationResult a = Success a | Failure [String] 
  deriving (Show, Eq)

-- Helper function to extract error messages
getErrors :: ValidationResult a -> [String]
getErrors (Success _) = []
getErrors (Failure errs) = errs

-- Helper function to check if validation succeeded
isValid :: ValidationResult a -> Bool
isValid (Success _) = True
isValid (Failure _) = False

-- ============================================================================
-- Basic Validation Primitives
-- ============================================================================

-- Example implementation provided
isNotEmpty :: String -> ValidationResult String
isNotEmpty "" = Failure ["Value cannot be empty"]
isNotEmpty s = Success s

-- TODO: Implement these following the same pattern:
-- | Succeeds if the string's length is at least minLen.
isMinLength :: Int -> String -> ValidationResult String
isMinLength minLen s
  | length s >= minLen = Success s
  | otherwise = Failure ["Value must be at least " ++ show minLen ++ " characters"]

-- | Succeeds if the string's length is at most maxLen.
isMaxLength :: Int -> String -> ValidationResult String
isMaxLength maxLen s
  | length s <= maxLen = Success s
  | otherwise = Failure ["Value must be at most " ++ show maxLen ++ " characters"]

-- | Succeeds if the string's length is exactly exactLen.
isExactLength :: Int -> String -> ValidationResult String
isExactLength exactLen s
  | length s == exactLen = Success s
  | otherwise = Failure ["Value must be exactly " ++ show exactLen ++ " characters"]

-- | Succeeds if the string contains only digits (0-9) and is not empty.
isNumeric :: String -> ValidationResult String
isNumeric s
  | not (null s) && all isDigit s = Success s
  | otherwise = Failure ["Value must be numeric"]

-- | Succeeds if the string contains only letters (a-z, A-Z) and is not empty.
isAlphabetic :: String -> ValidationResult String
isAlphabetic s
  | not (null s) && all isAlpha s = Success s
  | otherwise = Failure ["Value must contain only letters"]

-- | Succeeds if the string contains only letters and digits and is not empty.
isAlphanumeric :: String -> ValidationResult String
isAlphanumeric s
  | not (null s) && all isAlphaNum s = Success s
  | otherwise = Failure ["Value must be alphanumeric"]

-- | Succeeds if the string contains exactly one '@' and at least one '.' after the '@'.
isEmail :: String -> ValidationResult String
isEmail s =
  case break (== '@') s of
    (_, "") -> Failure ["Value must be a valid email address"]
    ("", _) -> Failure ["Value must be a valid email address"]
    (local, '@':domain) ->
      if null domain
        then Failure ["Value must be a valid email address"]
      else if '@' `elem` domain
        then Failure ["Value must be a valid email address"]
      else if '.' `notElem` domain
        then Failure ["Value must be a valid email address"]
      else Success s



-- | Succeeds if val is between minVal and maxVal (inclusive).
isInRange :: (Ord a, Show a) => a -> a -> a -> ValidationResult a
isInRange minVal maxVal val
  | val >= minVal && val <= maxVal = Success val
  | otherwise = Failure ["Value must be between " ++ show minVal ++ " and " ++ show maxVal]

-- | Transform all error messages using the given function.
mapError :: (String -> String) -> ValidationResult a -> ValidationResult a
mapError f (Failure errs) = Failure (map f errs)
mapError _ (Success a) = Success a

-- | Prepend the given context string and a colon to all error messages.
withContext :: String -> ValidationResult a -> ValidationResult a
withContext context = mapError (\e -> context ++ ": " ++ e)

-- | If the result is a Failure, return Success Nothing.
optional :: ValidationResult a -> ValidationResult (Maybe a)
optional (Success a) = Success (Just a)
optional (Failure _) = Success Nothing

-- | If the result is a Failure, return Success with the default value.
withDefault :: a -> ValidationResult a -> ValidationResult a
withDefault _ (Success a) = Success a
withDefault defaultVal (Failure _) = Success defaultVal

-- | If Success, apply the function to the value. If Failure, leave unchanged.
mapSuccess :: (a -> b) -> ValidationResult a -> ValidationResult b
mapSuccess f (Success a) = Success (f a)
mapSuccess _ (Failure errs) = Failure errs

-- | If the first validation succeeds, apply the second validator to its value.
andThen :: ValidationResult a -> (a -> ValidationResult b) -> ValidationResult b
andThen (Success a) validator = validator a
andThen (Failure errs) _ = Failure errs

-- | Apply all validators to the input (each gets the original input).
-- | Apply all validators to the input (each gets the original input).
chainValidations :: [a -> ValidationResult a] -> a -> ValidationResult a
chainValidations validators input =
  let
    -- Apply each validator and get a list of results
    results = map ($ input) validators
    -- Collect all error messages from the failure results
    errors = concatMap getErrors results
  in
    -- If the list of all errors is empty, the validation succeeds
    if null errors
      then Success input
      else Failure errors

-- | Apply a validator, then transform the success value.
validateAndTransform :: (a -> ValidationResult b) -> (b -> c) -> a -> ValidationResult c
validateAndTransform validator transformer input =
  validator input `andThen` (Success . transformer)

-- | All validations must succeed (AND logic).
allOf :: [ValidationResult a] -> ValidationResult [a]
allOf results =
  let (successes, errors) = foldr collect ([], []) results
  in if null errors
       then Success successes
       else Failure errors
  where
    collect (Success a) (s, e) = (a : s, e)
    collect (Failure errs) (s, e) = (s, errs ++ e)

-- | At least one validation must succeed (OR logic).
anyOf :: [ValidationResult a] -> ValidationResult a
anyOf [] = Failure ["No validations given"]
anyOf results = go results []
  where
    go [] errs = Failure errs
    go (Success a : _) _ = Success a
    go (Failure e : xs) errs = go xs (errs ++ e)



-- | Both validations must succeed to get a tuple. Otherwise, combine all errors.
both :: ValidationResult a -> ValidationResult b -> ValidationResult (a, b)
both (Success a) (Success b) = Success (a, b)
both (Failure errs1) (Failure errs2) = Failure (errs1 ++ errs2)
both (Failure errs) (Success _) = Failure errs
both (Success _) (Failure errs) = Failure errs

-- | Apply the validator to each element in the list. All must succeed.
validateEach :: (a -> ValidationResult b) -> [a] -> ValidationResult [b]
validateEach validator inputs =
  let results = map validator inputs
  in allOf results

-- | At least n validations must succeed.
validateAtLeast :: Int -> [ValidationResult a] -> ValidationResult [a]
validateAtLeast n results =
  let (successes, errors) = foldr collect ([], []) results
  in if length successes >= n
       then Success successes
       else Failure ["Not enough valid values: need " ++ show n ++ " but got " ++ show (length successes)]
  where
    collect (Success a) (s, e) = (a : s, e)
    collect (Failure errs) (s, e) = (s, errs ++ e)
