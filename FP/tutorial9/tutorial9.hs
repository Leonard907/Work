-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 9(11-15 Nov.)
module Tutorial9 where

import Data.List
import Test.QuickCheck
import Data.Char


sizeCheck n = quickCheckWith (stdArgs {maxSize = n})

-- Type declarations

type FSM q = ([q], Alphabet, [q], [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      [0],
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      ['B'],
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [[0]],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> [q]
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]

states (k, _, _, _, _) = k
alph   (_, a, _, _, _) = a
start  (_, _, s, _, _) = s
final  (_, _, _, f, _) = f
trans  (_, _, _, _, t) = t


-- 2.
delta :: (Eq q) => FSM q -> [q] -> Char -> [q]
delta m s symbol = [t | s' <- s, (f , sym, t) <- trans m, s' == f, sym == symbol]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m [] = or [elem s' $ final m | s' <- start m]
accepts m@(k,a,s,f,t) (x:xs) = accepts (k,a,(delta m s x),f,t) xs

acceptsFrom :: (Eq q) => FSM q -> [q] -> String -> Bool
acceptsFrom m q "" = or[ r `elem` final m | r <- q ]
acceptsFrom m q (x:xs) = acceptsFrom m (delta m q x) xs

prop_accepts :: (Eq q) => FSM q -> String -> Bool
prop_accepts m s = accepts m s == acceptsFrom m (start m) s


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = nub.sort


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m s c = canonical $ delta m s c

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m qs = canonical ([ddelta m q s | q <- qs, s <- alph m] ++ qs)


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m s 
    | and [elem s' s | s' <- next m s] = s
    | otherwise = canonical (s ++ reachable m (next m s))

-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m s = [s' | s' <- s, f <- final m, elem f s']

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m s = [(s',sym,ddelta m s' sym) | s' <- s, sym <- alph m]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (starts m,alph m,[start m],dfinal m $ starts m,
                    dtrans m $ starts m)
    where starts mc = reachable mc [start mc]


-- Optional Material

-- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

--11.

charFSM :: Char -> FSM Bool
charFSM c = ([True,False],[c],[False],[True],[(False,c,True)])

emptyFSM :: FSM ()
emptyFSM = ([],[],[],[],[])

prop_empty :: String -> Bool
prop_empty s = accepts emptyFSM s == False

--12.
f = stringFSM ""

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')                             
concatFSM a b 
    | length (trans a) == 0 = (map Right (states b),
                           alph b,
                           map Right (start b),
                           map Right (final b),
                           map toRight (trans b)) 
    | length (trans b) == 0 = (map Left (states a),
                           alph a,
                           map Left (start a),
                           map Left (final a),
                           map toLeft (trans a))                      
    | otherwise = ( (map Left (states a) ++ map Right (states b)), 
                    canonical (alph a ++ alph b),
                    (map Left (start a)),
                    (map Right (final b)),
                    (map toLeft (trans a)) ++ -- Transformations in the first machine
                    (map toRight (trans b)) ++ -- Trans in the second machine
                    [(Left s,sym,Right s') | (s,sym,f) <- trans a, s' <- start b, elem f (final a)]                      )

toLeft (x,y,z)  = (Left x,y,Left z)
toRight (x,y,z) = (Right x,y,Right z)

prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o =
  accepts fsm (s ++ t)
  && (accepts fsm u == (s ++ t == u))
  where
  fsm = concatFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

--13.

intFSM :: Ord q => FSM q -> FSM Int
intFSM a = ([0..(length (states a) - 1)],
            alph a,
            [n | (s,n) <- pos a, elem s (start a)],
            [n | (s,n) <- pos a, elem s (final a)],
            [(lookFor s,sym,lookFor f) | (s,sym,f) <- trans a])
    where pos m = zip (states m) [0..]
          lookFor sth = head [n | (s',n) <- pos a, sth == s']

lookUp :: Eq q =>  q -> [(q,Int)] ->  Int
lookUp q' qis =  the [ i | (q,i) <- qis, q == q' ]
  where
  the [q] = q

stringFSM :: String -> FSM Int
stringFSM str = ([0..(length str)],
                 str,
                 [0],
                 [length str],
                 [(n,c,n+1) | (n,c) <- zip [0..(length str)] str])

prop_stringFSM m n =
  accepts a s
  && accepts a t == (s == t)
  where
  a = stringFSM s
  s = safeString m
  t = safeString n

--14.

completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM m@(k,a,s,f,t) = (map Just k ++ [Nothing],a,map Just s,map Just f,
                             complement m)
    where complement mh@(k',a',_,_,t') = [(Just s',sym,Nothing) | sym <- a', s' <- k', not (inTrans sym s' mh)]
                                       ++ (map (\(x,y,z) -> (Just x,y,Just z)) t')
          inTrans symbol st (_,_,_,_,ts) = (length . filter (\(x,y,z) -> y == symbol && x == st)) ts >= 1

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Maybe q, Maybe q')
unionFSM a b = (allStates a b,
                allAlph a b,
                [(s1,s2) | (s1,s2) <- allStates a b, 
                elem s1 (start (completeFSM a)) && elem s2 (start (completeFSM b))],
                [(s1,s2) | (s1,s2) <- allStates a b, 
                elem s1 (final (completeFSM a)) || elem s2 (final (completeFSM b))],
                [((s1,s2),sym,(f1,f2)) | (s1,s2) <- allStates a b, sym <- allAlph a b,
                f1 <- safe $ ddelta (completeFSM a) [s1] sym, f2 <- safe $ ddelta (completeFSM b) [s2] sym]
              )

safe :: (Ord q) => [Maybe q] -> [Maybe q]
safe list 
    | length list == 0 = [Nothing]
    | otherwise        = list

allStates :: (Ord q, Ord q') => FSM q -> FSM q' -> [(Maybe q, Maybe q')]
allStates a b = [(s1,s2) | s1 <- states (completeFSM a), s2 <- states (completeFSM b)]

allAlph :: (Ord q, Ord q') => FSM q -> FSM q' -> [Char]
allAlph a b = canonical (alph a ++ alph b)
 
        
prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  accepts fsm u == (accepts a u || accepts b u)
  && accepts fsm s
  && accepts fsm t
  where
  fsm = unionFSM a b
  a = stringFSM s
  b = stringFSM t
  c = stringFSM u
  s = safeString m
  t = safeString n
  u = safeString o

--15.

star :: (Ord q) => FSM q -> FSM q
star m@(k,a,s,f,t) = (k,a,s,f ++ s,t ++ [(f',sym,s') | f' <- f, sym <- a, s' <- ddelta m s sym])

prop_star :: String -> Int -> Bool
prop_star m n =
  accepts fsm (concat (replicate i s))
  where
  fsm = star (stringFSM s)
  s = safeString m
  i = abs n


--16.

complementFSM :: (Ord q) => FSM q -> FSM (Maybe q)
complementFSM m = undefined
           
prop_complement :: String -> String -> Bool
prop_complement m n =
  not (accepts fsm s)
  && accepts fsm t == not (s == t)
  where
  fsm = complementFSM (stringFSM s)
  s = safeString m
  t = safeString n

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
intersectFSM = undefined
                
prop_intersectFSM1 m n =
  accepts fsm s
  && accepts fsm t == (s == t)
  where
  fsm = intersectFSM a a
  a = stringFSM s
  s = safeString m
  t = safeString n

prop_intersectFSM2 m n o =
  accepts fsm u == (accepts a u && accepts b u)
  where
  fsm = intersectFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

prop_intersectFSM3 m n o =
  accepts fsm s
  && accepts fsm u == accepts a u
  where
  fsm = intersectFSM a (unionFSM a b)
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o
