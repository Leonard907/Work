-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)

module Main where

import LSystem
import Test.QuickCheck
    
pathExample1 = (Go 40 :#: Turn 120 :#: Go 30 :#: Turn 120 :#:  Go 30)
pathExample2 = join (map inDirection [20,40..360])
    where 
        inDirection angle = Branch (Turn angle :#: Go 100)
    
-- helper function to remove parentheses
    
pretty :: Command -> String
pretty (c1 :#: c2) = pretty c1 ++ " :#: " ++ pretty c2
pretty c = show c
    
-- 1a. split
split :: Command -> [Command]
split (c1 :#: c2) = split c1 ++ split c2
split Sit = []
split c1 = [c1]
    
-- 1b. join
join :: [Command] -> Command
join [x] = x
join c = foldl1 (\x y -> x :#: y) c
    
-- 1c. equivalent
-- equivalent 
equivalent :: Command -> Command -> Bool
equivalent c1 c2 = split c1 == split c2
    
-- 1d. testing join and split
-- prop_split_join 
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c
    
-- prop_split
prop_split :: Command -> Bool
prop_split c = length (split c) == length (filter (\x -> x /= Sit) (split c))
    
    
-- 2a. copy
copy :: Int -> Command -> Command
copy n c = join [c | _ <- [1..n]]
    
-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dis = copy 5 ((Go dis) :#: (Turn 72.0))
    
-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon dis sides = copy sides ((Go dis) :#: (Turn (fromIntegral (360 `div` sides))))
    
    
-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral start cycle inc turn = join [(Go ((fromIntegral n) * inc + start)) :#: (Turn turn) | n <- [0..cycle-1]]
    
    
-- 4. optimise
-- Remember that Go does not take negative arguments.
    
optimise :: Command -> Command
optimise c = join (filter (\x -> x /= Sit) (simplify (change (split c))))
    where
        change c = [check e | e <- c]
        simplify [] = []
        simplify [x] = [x]
        simplify (x:y:xs) 
            | length (combine x y) < 2 = simplify (combine x y ++ xs)
            | otherwise = [x] ++ simplify (y:xs)
        combine (Go a) (Go b) = [check (Go (a + b))]
        combine (Turn a) (Turn b) = [check (Turn (a + b))]
        combine c Sit = [c]
        combine Sit c = [c]
        combine c1 c2 = [c1, c2]
        check (Go 0) = Sit
        check (Turn 0) = Sit
        check c = c


    
-- L-Systems
    
-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
        f 0 = Go 10
        f x = g (x - 1) :#: n :#: f (x - 1) :#: n :#: g (x - 1)
        g 0 = Go 10
        g x = f (x - 1) :#: p :#: g (x - 1) :#: p :#: f (x - 1)
        n = Turn 60
        p = Turn (-60)
    
-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: p :#: p :#: f x :#: p :#: p :#: f x 
    where
        f 0 = Go 10
        f x = f (x - 1) :#: n :#: f (x - 1) :#: p :#: p :#: f (x - 1) :#: n :#: f (x - 1)
        n = Turn 60
        p = Turn (-60)
    
    
-- 7. hilbert

    
    --------------------------------------------------
    --------------------------------------------------
    ---------------- Optional Material ---------------
    --------------------------------------------------
    --------------------------------------------------
    
-- Bonus L-Systems
    
peanoGosper :: Int -> Command
peanoGosper x = f x
    where
        f 0 = Go 10
        f x = f (x - 1) :#: n :#: g (x - 1) :#: n :#: n 
              :#: g (x - 1) :#: p :#: f (x - 1) :#: p 
              :#: p :#: f (x - 1) :#: f (x - 1) :#: p :#: g (x - 1) :#: n
        g 0 = Go 10
        g x = p :#: f (x - 1) :#: n :#: g (x - 1) :#: g (x - 1) 
            :#: n :#: n :#: g (x - 1) :#: n
            :#: f (x - 1) :#: p :#: p :#: f (x - 1) :#: p :#: g (x - 1)
        n = Turn 60
        p = Turn (-60)
    
cross :: Int -> Command
cross x = f x :#: p :#: f x :#: p :#: f x :#: p :#: f x
    where
        f 0 = Go 10
        f x = f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: f (x - 1)
            :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) 
        n = Turn 90
        p = Turn (-90)        
    
branch :: Int -> Command
branch x = g x
    where
        g 0 = GrabPen red :#: Go 10
        g x = f (x - 1) :#: p :#: Branch (Branch (g (x - 1)) :#: n :#: g (x - 1)) :#: n :#: f (x - 1)
            :#: Branch (n :#: f (x - 1) :#: g (x - 1)) :#: p :#: g (x - 1)
        f 0 = GrabPen blue :#: Go 10
        f x = f (x - 1) :#: f (x - 1)
        n = Turn 22.5
        p = Turn (-22.5)
    
thirtytwo = undefined
    
    
main :: IO ()
main = display (branch 5)