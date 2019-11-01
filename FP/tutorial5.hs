-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where

import Data.Char
import Data.Ratio
import Test.QuickCheck
import Data.List

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles = map (*2)
    

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds = map (\x -> (fromIntegral x) / 100)
    

-- c.
uppers :: String -> String
uppers = map toUpper

--d.
uppersComp :: String -> String
uppersComp str = [toUpper x | x <- str]

prop_uppers :: String -> Bool
prop_uppers x = uppers x == uppersComp x

-- 2. Filter

-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
above :: Int -> [Int] -> [Int]
above x = filter (>x)

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(x,y) -> x /= y)

-- d.
rmChar :: Char -> String -> String
rmChar c = filter (/=c)

rmCharComp :: Char -> String -> String
rmCharComp c str = [x | x <- str, x /= c]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c str = rmChar c str == rmCharComp c str

-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' ints = map (*2) $ filter (>3) ints

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse $ filter (even . length) strs

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs

-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec _ [] = []
rmCharsRec [] str = str
rmCharsRec (x:xs) str = rmCharsRec xs (rmChar x str)
    

rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr rmChar str chars

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str

type Matrix = [[Rational]]

m11 :: Matrix
m11 = [[1,2,3],[4,5,6]]

-- 5
-- a.
allRec :: (a -> Bool) -> [a] -> Bool
allRec _ [] = True
allRec f (x:xs) = f x && allRec f xs

allMap :: (a -> Bool) -> [a] -> Bool
allMap f = and . map f

allFilter :: (a -> Bool) -> [a] -> Bool
allFilter f = (\x -> length x == length (filter f x))

uniform :: [Int] -> Bool
uniform ints = all (==head ints) ints

-- b.
valid :: Matrix -> Bool
valid [] = False
valid matrix = uniform . map length $ matrix

-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length m

matrixHeight :: Matrix -> Int
matrixHeight m = length $ head m

plusM' :: Matrix -> Matrix -> [[Float]]
plusM' m1 m2
    | (matrixWidth m1 /= matrixWidth m2) || (matrixHeight m1 /= matrixHeight m2) = error "Input not in correct format"
    | otherwise = zipWith plusRow (toFloat m1) (toFloat m2)
         where plusRow = zipWith (+)
               toFloat = map (map fromRational)

plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2
    | (matrixWidth m1 /= matrixWidth m2) || (matrixHeight m1 /= matrixHeight m2) = error "Input not in correct format"
    | otherwise = zipWith plusRow m1 m2
         where plusRow = zipWith (+)

-- 7.
dot :: [Rational] -> [Rational] -> Rational
dot r1 r2 = sum (zipWith (*) r1 r2)

dot' :: Num a => [a] -> [a] -> a
dot' r1 r2 = sum (zipWith (*) r1 r2)

timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 
    |  (matrixWidth m1 /= matrixHeight m2) || (matrixHeight m1 /= matrixWidth m2) = error "Input not in correct format"
    | otherwise = [[dot i j | i <- transpose m2] | j <- m1]

timesM' :: Matrix -> Matrix -> [[Float]]
timesM' m1 m2 
    |  (matrixWidth m1 /= matrixHeight m2) || (matrixHeight m1 /= matrixWidth m2) = error "Input not in correct format"
    | otherwise = [[dot' i j | i <- transpose (toFloat m2)] | j <- toFloat m1]
       where toFloat = map (map fromRational)

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [fst x `f` snd x | x <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-- -- 9.

-- Dimension of a matrix
dim :: Matrix -> Int
dim = matrixWidth

-- Entry at position (i,j)
entry :: Int -> Int -> Matrix -> Rational
entry i j m = (m !! (i - 1)) !! (j - 1)

-- Mapping matrices
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = map (map f)

-- Zipping matrices
zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f m1 m2 = zipWith (zipWith f) m1 m2

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Matrix
signMatrix d = [[(-1)^(i+j) | i <- [1..d]] | j <- [1..d]]

-- Submatrices of element a_ij
sub :: Int -> Int -> Matrix -> Matrix
sub row col m = [[(m !! (j - 1)) !! (i - 1) | i <- [1..dim m], i /= col] | j <- [1..dim m], j /= row] -- excluding elements of row i and col j.

-- Get determinant
determinant :: Matrix -> Rational
determinant m
    | length m == 1 = head (head m) -- one element then det(m) = value of this element
    | length m == 2 = ((m !! 0) !! 0) * ((m !! 1) !! 1) - ((m !! 0) !! 1) * ((m !! 1) !! 0) -- det(m) of matrix of dimension 2 can be directly calculated
    | otherwise = sum [(entry 1 j m) * (determinant (sub 1 j m)) * ((-1)^(1 + j)) | j <- [1..length m]] -- goes into sub-matrices and calculate their values

-- The cofactor matrix of the original one
cofactors :: Matrix -> Matrix
cofactors m = [[determinant (sub i j m) | j <- [1..dim m]] | i <- [1..dim m]] -- cofactor matrix use for inverse

-- Get inverse!!
inverse :: Matrix -> Matrix
inverse m
    | matrixWidth m /= matrixHeight m = error "Input not square matrix"
    | determinant m == 0 = error "Singular Matrix"
    | otherwise = transpose (mapMatrix (/(determinant m)) (zipMatrix (*) (cofactors m) (signMatrix (dim m))))

-- Formatting output
format :: Matrix -> IO ()
format = putStr . unlines . map show

