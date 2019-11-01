module Main where

import LSystem

split :: Command -> [Command]
split (c1 :#: c2) = split c1 ++ split c2
split Sit = []
split c1 = [c1]
    
join :: [Command] -> Command
join [] = Sit
join [x] = x
join c = foldl1 (\x y -> x :#: y) c

copy :: Int -> Command -> Command
copy n c = join [c | _ <- [1..n]]

step :: Float -> Float
step radius = radius * pi / 1800

maze :: Float  -> Int -> Command
maze inc cycle = f cycle
    where
        f 0 = p :#: p :#: copy (3480 * (cycle + 1)) (Go (step inc) :#: Turn ((-0.1) / (fromIntegral (cycle + 1))))
        f x = copy (2250 * (cycle + 1 - x)) (Go (step inc) :#: Turn (0.1 / (fromIntegral (cycle + 1 - x))))
            :#: Branch (p :#: Go inc :#: Branch (p :#: copy (1100 * (cycle + 2 - x)) (Go (step inc) :#: Turn ((-0.1) / (fromIntegral (cycle + 2 - x)))))
            :#: Branch (n :#: f (x - 1)))
        n = Turn 90
        p = Turn (-90)

main :: IO ()
main = display (maze 10 3)
