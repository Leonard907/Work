module BalancedTree
  (Set(Nil,Node),empty,insert,set,element,equal,check) where
import Test.QuickCheck


-- depth of a tree is equal to max string of elements
type  Depth  =  Int
data  Set a  =  Nil | Node (Set a) a (Set a) Depth deriving (Show)

node :: Set a -> a -> Set a -> Set a
node l x r  =  Node l x r (1 + (depth l `max` depth r))

depth :: Set a -> Int
depth Nil  =  0
depth (Node _ _ _ d) = d

list :: Set a -> [a]
list Nil             =  []
list (Node l x r _)  =  list l ++ [x] ++ list r

invariant :: Ord a => Set a -> Bool
invariant Nil   =  True
invariant (Node l x r d)  =
  invariant l && invariant r &&
  and [ y < x | y <- list l ] &&
  and [ y > x | y <- list r ] &&
  abs (depth l - depth r) <= 1 &&
  d == 1 + (depth l `max` depth r)

empty :: Set a
empty  =  Nil

insert :: Ord a => a -> Set a -> Set a
insert x Nil  =  node empty x empty
insert x (Node l y r _)
  | x == y     =  node l y r
  | x < y      =  rebalance (node (insert x l) y r)
  | x > y      =  rebalance (node l y (insert x r))

set :: Ord a => [a] -> Set a
set  =  foldr insert empty

-- all variables are in increasing order
rebalance :: Set a -> Set a
rebalance (Node (Node a x b _) y c _)
  | depth a >= depth b && depth a > depth c
  = node a x (node b y c) -- identify the greatest depth branch, attach it directly \n
                          -- to the root and move other branches to the other side
rebalance (Node a x (Node b y c _) _)
  | depth c >= depth b && depth c > depth a
  = node (node a x b) y c -- same as above, this time look from the right
rebalance (Node (Node a x (Node b y c _) _) z d _)
  | depth (node b y c) > depth d
  = node (node a x b) y (node c z d) -- identify a branch on the left which has an unnecessary pivot
rebalance (Node a x (Node (Node b y c _) z d _) _)
  | depth (node b y c) > depth a
  = node (node a x b) y (node c z d) -- same as above, this time on the right
rebalance a  =  a

element :: Ord a => a -> Set a -> Bool
x `element` Nil  =  False
x `element` (Node l y r _)
  | x == y     =  True
  | x < y      =  x `element` l
  | x > y      =  x `element` r

equal :: Ord a => Set a -> Set a -> Bool
s `equal` t  =  list s == list t

prop_invariant :: [Int] -> Bool
prop_invariant xs  =  invariant s
  where
  s = set xs

prop_element :: [Int] -> Bool
prop_element ys  =
  and [ x `element` s == odd x | x <- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_invariant >>
  quickCheck prop_element

-- Prelude SetBalancedTree> check
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.