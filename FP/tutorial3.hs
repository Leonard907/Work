-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
    | even x = div x 2 : continue
    | otherwise = continue
    where continue = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvensRec xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs)
    | x <= hi && x >= lo = x : continue
    | otherwise = continue
    where continue = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
    | x > 0 = 1 + con
    | otherwise = con
    where con = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
    | isDigit x = digitToInt x * multDigitsRec xs
    | otherwise = multDigitsRec xs
    

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs
    | length list > 0 = head list
    | otherwise = ch
    where list = [snd x | x <- xs, fst x == ch] 

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch (x:xs)
    | fst x == ch = snd x
    | otherwise = lookUpRec ch xs
    

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUpRec c k == lookUp c k


-- 6.

encipher :: Int -> Char -> Char
encipher k ch
    | isDigit ch = ch
    | ord ch + k > 90 = chr $ 65 + ord ch + k - 91
    | otherwise = chr $ ord ch + k


-- 7.

normalize :: String -> String
normalize [] = []
normalize (x:xs)
    | isAlpha x = toUpper x : normalize xs
    | isDigit x = x : normalize xs
    | otherwise = normalize xs


encipherStr :: Int -> String -> String
encipherStr k str = map (encipher k) (normalize str)


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey keys = [(snd x, fst x) | x <- keys]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) = (snd x, fst x) : reverseKeyRec xs
    

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey keys = reverseKey keys == reverseKeyRec keys


-- 9.

decipher :: Int -> Char -> Char
decipher k ch
    | isDigit ch = ch
    | ord ch - k < 65 = chr $ 91 - 65 + ord ch - k
    | otherwise = chr $ ord ch - k

decipherStr :: Int -> String -> String
decipherStr k str = map (decipher k) str

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains str substr = or [isPrefixOf substr $ drop n str | n <- [0..length str]]

-- 11.

candidates :: String -> [(Int, String)]
candidates str = [(x, decipherStr x str) | x <- [1..25], contains (decipherStr x str) "THE" || contains (decipherStr x str) "AND"]                


-- 12.

splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive str
    | length str >= 5 = take 5 str : droped
    | otherwise = (take 5 str ++ replicate (5 - length str) 'X') : droped
    where droped = splitEachFive (drop 5 str)
prop_transpose :: String -> Bool
prop_transpose str = splitEachFive str == transpose (transpose (splitEachFive str))

prop_transpose' :: [String] -> Bool
prop_transpose' lstr = lstr == transpose (transpose lstr)

-- one counterexample: [""]

-- 13.
encrypt :: Int -> String -> String
encrypt s = concat . transpose . splitEachFive . encipherStr s 


-- 14.

splitRandom :: Int -> String -> [String]
splitRandom _ [] = []
splitRandom n str = take n str : (splitRandom n (drop n str))

decrypt :: Int -> String -> String
decrypt s str = decipherStr s (concat $ transpose $ splitRandom ((length str) `div` 5) str)
