-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, even x]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x <= hi, x >= lo]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = sum [1 | x <- list, x > 0]
-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = sum [1 | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9 ^ countDigits xs


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : [toLower c | c <- xs]


-- 6. title

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise x : [manipulate c | c <- xs]
    where manipulate char
              | length char > 3 = capitalise char
              | otherwise = lowercase char



-- 7. signs

sign :: Int -> Char
sign i 
    | i >= 1 && i <= 9 = '+'
    | i == 0 = '0'
    | i <= (-1) && i >= (-9) = '-'
    | otherwise = error "Invalid"


signs :: [Int] -> String
signs xs = [sign x | x <- xs, x <= 9, x >= (-9)]

-- 8. score

score :: Char -> Int
score x 
    | elem (toLower x) ['a','e','i','o','u'] && isUpper x = 3
    | elem (toLower x) ['a','e','i','o','u'] || isUpper x = 2
    | isLetter x = 1
    | otherwise = 0


totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, isLetter x]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1

-- Tutorial Activity
-- 10. pennypincher

-- Auxiliary function
discount :: Int -> Int
discount price = round $ fromIntegral price * 9 / 10-- List-comprehension version.

pennypincher :: [Int] -> Int
pennypincher prices = sum [discount x | x <- prices, discount x <= 19900]

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [x | x <- xs, x > 0]

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [x | x <- words, length x == len, x !! pos == letter]



-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [snd p | p <- zip str [0..length str - 1], fst p == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str


-- 13. contains

contains :: String -> String -> Bool
contains str substr = or [isPrefixOf substr $ drop n str | n <- [0..length str]]

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 
    | length str1 == length str2 = True
    | length str1 < length str2 = not (contains str1 str2)
    | otherwise = not (contains str2 str1)
