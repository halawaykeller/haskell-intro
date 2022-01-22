-- {-# LANGUAGE OverloadedStrings #-}

module Wordle
    ( wordle
    ) where

import Words
import qualified Data.Char as C
-- import qualified Data.Text as T
-- import Data.Text (Text)

-- Rules of Wordle
-- Takes a 5 letter guess
-- Must be a real word
-- Max 6 tries to match the word
-- Have we already guessed the letter?
-- Is the character a letter? (no numbers)
-- Checks each letter 
-- Two checks, is this letter in the word?
-- Is this letter in the right spot?
-- Extra: today's word vs yesterday's word

-- What do we need to do this exercise?
-- Lists, text, string matching, knowing which index the letters are in
-- Some state tracking for how many tries
-- Printing, showing, error handling (Maybe, Either)

-- correct letter wrong position
-- correct letter right postion
-- incorrect letter

-- | Data Types
data WordleGame = WordleGame {
    isMatch :: Bool,
    attempts:: Int,
    previousGuesses :: [GuessedLetters]
} deriving (Show, Read, Eq)

data GuessedLetters = GuessedLetters {
    letter :: Char,
    position :: Int,
    correctLetter :: Bool,
    correctPosition :: Bool
} deriving (Show, Read, Eq)

-- shift -- correct word
-- spice -- guess
-- raspy -- guess

-- [(s, s), (h, p), (i, i), (f, c), (t, e)]
-- checks position 
-- [(s, r), (h, a), (i, s), (f, y), (t, y)]


wordle :: IO ()
wordle = do
    print $ makeGuess "River"
    print $ makeGuess "Sleep"
    print $ makeGuess "Crown"
    print $ makeGuess "Crime"
    print $ makeGuess "Brain"
    print $ makeGuess "Child"

-- | Constants
wordOfTheDay :: Int 
wordOfTheDay = 50

-- | Accesssing an Array
-- Gets a word from the cyclic dictionary
-- If the index is larger than the pure list, we cycle the list
-- We guard against a negative index by ensuring our input is always larger that 0
-- Without max 0 1 this function would create a runtime error
-- We could refactor this to use an Either if we wanted to throw an error
getWordOfTheDay :: Int -> String
getWordOfTheDay i = cycle dictionary !! max 0 i

-- Some text processing, all the words in our dictionary are upper case ¯\_(ツ)_/¯
-- Weird choice, but that's what I copied from the internet
-- This is annoying so let's lower case all those strings
-- Note the eta reduction
processedDictionary :: [String] 
processedDictionary = map (map C.toLower) dictionary 

isGuessRealWord :: String -> Bool
isGuessRealWord guess = guess `elem` processedDictionary 

-- | Game Functions

makeGuess :: String -> Maybe WordleGame
makeGuess guess = undefined

checkGuessedLetters :: String -> String -> [GuessedLetters]
checkGuessedLetters guess word = undefined 
    
checkMembership :: Char -> String -> Bool
checkMembership letter word = letter `elem` word

-- Look at this linter suggestion and take it apart
checkPositions :: String -> String -> [(Char, Bool)]
checkPositions guess word = zip guess isCorrectPosition
    where 
        -- Where clauses are locally scoped functions
        -- But they can take their arguments from the function they are scoped within
        isCorrectPosition :: [Bool]
        isCorrectPosition = map compareLetters (zip guess word) -- zipWith (==) guess word -> zipwWith (==)
        -- Currying / uncurrying
        compareLetters :: (Char, Char) -> Bool
        compareLetters letters = uncurry (==) letters






