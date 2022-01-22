module Syntax where

-- import qualified Data.Map as M
-- | Types
-- Type Declarations

numberOne :: Int
numberOne = 1

strings :: String 
strings = "Hello"

truth :: Bool 
truth = True 

arrayOfInt :: [Int]
arrayOfInt = [1,2,3]

-- | Functions
-- Function Arguments
-- Currying
-- Function Application

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers i j = i + (addOne j) 
    where 
        addOne :: Int -> Int 
        addOne i = (+) i 1

-- | Control Flow
-- If / Then / Else
-- Gaurds
-- Case statements

isYellow :: String -> Int 
isYellow guess = if guess == "Yellow" then 1 else 2

isBlue :: String -> String 
isBlue guess 
    | guess == "Yellow" = "Green"
    | guess == "Red" = "Purple"
    | guess == "Blue" = "Blue"
    | otherwise = "Not primary"

makeColor :: String -> Maybe String 
makeColor guess = case guess of
    "Purple" -> Just "Purple"
    "Blue" -> Just "Blue"
    _ -> Nothing

checkIfColor :: String -> String 
checkIfColor color = case makeColor color of
    Just c -> c
    Nothing -> "Not a color"

-- maybeString :: Maybe String 
-- maybeString = Just "hello"
-- maybeString = Nothing

-- | Pattern Matching


-- | Loops
-- Recursion


-- | Data Structures
-- Lists, Maps, Tuples

-- | Error Handling
-- Maybe / Either

-- | Do Syntax

-- | IO

-- | User Defined Types
