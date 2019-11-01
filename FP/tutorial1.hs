-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 1
--
-- Week 1(16-20 Sep.)
--
-- Insert your name and matriculation number here:
-- Name: Leonard Ou
-- Nr. s1970716:
-- Exercise 1:
-- 1.b 2.a 3.b 4.b 5.a 6.a,b


import Test.QuickCheck

-- Exercise 3:

double :: Int -> Int
double x = x + x 
-- addition or subtraction good. Multiplication sometimes not supported

square :: Int -> Int
square x = x * x

-- Exercise 4:

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = a^2 + b^2 == c^2


-- Exercise 5:

leg1 :: Int -> Int -> Int
leg1 x y = x^2 - y^2

leg2 :: Int -> Int -> Int
leg2 x y = 2 * x * y

hyp :: Int -> Int -> Int
hyp x y = x^2 + y^2


-- Exercise 6:

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)
