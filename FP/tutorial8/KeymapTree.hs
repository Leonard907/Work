-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)
-- Indexed data represented as a tree


module KeymapTree 

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a) 

-- A test tree

testTree :: Keymap Int Int
testTree = Node 3 20 (Node 1 10 Leaf Leaf)
                     (Node 5 30 Leaf 
                               (Node 7 40 Leaf Leaf ))
                      
testTree2 :: Keymap Int Int
testTree2 = Node 2 20 (Node 4 10 Leaf Leaf)
                      (Node 6 30 Leaf 
                                (Node 8 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + max (size left) (size right) 

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node x y left right) = toList left ++ [(x,y)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get _ Leaf = Nothing
get key (Node k v left right) | key == k  = Just v
                              | key < k  = get key left
                              | otherwise = get key right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList ((k,a):xs) = set k a (fromList xs)

fromListFold :: Ord k => [(k,a)] -> Keymap k a
fromListFold list = foldr (\(k,a) l -> set k a l) Leaf list

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromListFold zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromListFold zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 13

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT key (Node k v left right) | key <= k  = filterLT key left
                                   | otherwise = Node k v left (filterLT key right)

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT key (Node k v left right) | key >= k  = filterGT key right
                                   | otherwise = Node k v (filterGT key left) right

-- Exercise 14

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge km1 Leaf = km1
merge km1 (Node k v left right) = merge (merge (set k v km1) left) right

merge' :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge' km1 Leaf = km1
merge' Leaf km2 = km2
merge' (Node k1 v1 left1 right1) (Node k2 v2 left2 right2)
    | k1 == k2  = Node k1 v1 (merge left1 left2) (Node k2 v2 Leaf (merge right1 right2))
    | k1 < k2   = Node k1 v1 (merge left1 (filterLT k1 left2)) (merge right1 (filterGT k1 right2))
    | otherwise = Node k1 v1 (merge (filterLT k2 left1) left2) (merge (filterGT k2 left1) right2)

prop_merge :: Ord k => Keymap k a -> Keymap k a -> Bool
prop_merge km1 km2 = undefined

-- Exercise 15

del :: Ord k => k -> Keymap k a -> Keymap k a
del key Leaf = Leaf
del key (Node k v left right)
    | key == k  = merge left right
    | key < k   = Node k v (del key left) right
    | otherwise = Node k v left (del key right)

-- Exercise 16

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf 
select pred (Node k v left right) 
    | pred v    = Node k v (select pred left) (select pred right)
    | otherwise = merge (select pred left) (select pred right)

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary