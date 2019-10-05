import Data.List (nub, sort)

-- The data type is Exp, which stands for expressions between variables
-- Valuation is defined type, where each variable is associated with a truth value
data Exp = Var String | T | F | Not Exp | Exp :|: Exp | Exp :&: Exp | Exp :->: Exp | Exp :<->: Exp deriving (Eq, Show)
type Valuation = [(String, Bool)]

-- print the expressions in written form
showExp :: Exp -> String
showExp (Var x) = x
showExp (T) = "T"
showExp (F) = "F"
showExp (Not p) = "(¬" ++ showExp p ++ ")"
showExp (p :|: q) = "(" ++ showExp p ++ " | " ++ showExp q ++ ")"
showExp (p :&: q) = "(" ++ showExp p ++ " & " ++ showExp q ++ ")"
showExp (p :->: q) = "(" ++ showExp p ++ " -> " ++ showExp q ++ ")"
showExp (p :<->: q) = "(" ++ showExp p ++ " <-> " ++ showExp q ++ ")"

-- find out if the target variable is in valuation system
lookUp :: String -> Valuation -> Bool
lookUp var v = the [ b | (x, b) <- v, x == var ]
    where the [x] = x
          the _ = error "lookUp: variable missing or not unique"

-- evaluate expressions
eval :: Valuation -> Exp -> Bool
eval v (Var x) = lookUp x v
eval v (T) = True
eval v (F) = False
eval v (Not p) = not (eval v p)
eval v (p :|: q) = eval v p || eval v q
eval v (p :&: q) = eval v p && eval v q
eval v (p :->: q) = not (eval v p) || eval v q
eval v (p :<->: q) = eval v p == eval v q

-- find all variables in the expression
findVariables :: Exp -> [String]
findVariables (Var x) = [x]
findVariables (F) = []
findVariables (T) = []
findVariables (Not p) = findVariables p
findVariables (p :|: q) = nub (findVariables p ++ findVariables q)
findVariables (p :&: q) = nub (findVariables p ++ findVariables q)
findVariables (p :->: q) = nub (findVariables p ++ findVariables q)
findVariables (p :<->: q) = nub (findVariables p ++ findVariables q)

-- generate all pairs of truth values of the variables
generateValuations :: [String] -> [Valuation]
generateValuations [] = [[]]
generateValuations (x:xs) = [(x, False):e | e <- generateValuations xs ] ++ [(x, True ):e | e <- generateValuations xs ]

-- Test if the expression given is satisfiable or not (i.e. a set of truth values returns True)
isSatisfiable :: Exp -> Bool
isSatisfiable e = or [eval v e | v<-(generateValuations $ findVariables e)]

phi :: Exp
phi = (Var "A" :|: Not (Var "C") :|: Not (Var "D"))
      :&: (Var "A" :|: Var "C" :|: Not (Var "D")) :&: (Var "A" :|: Var "D")
      :&: (Var "B" :|: Not (Var "C"))
      :&: Var "A"
val1 :: Valuation
val1 = [("A", True), ("B", False), ("C", True), ("D", False)]
exp1 :: Exp
exp1 = Var "A" :<->: (Var "B" :->: (Var "C" :|: Var "D"))
val2 :: Valuation
val2 = [("P", True), ("Q", True), ("R", False)]
exp2 :: Exp
exp2 = (Var "P" :->: (Var "Q" :->: Var "R")) :&: (Not (Var "P") :->: Var "R") :<->: (Var "P" :->: Var "R")





