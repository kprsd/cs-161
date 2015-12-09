import System.IO
import Data.Maybe
import Data.Char
import Data.List

data VRArithExpr = Var (String, Int) | Number Int | Frac VRArithExpr VRArithExpr | Plus VRArithExpr VRArithExpr | Mult VRArithExpr VRArithExpr
    deriving (Show, Eq)


instance Ord VRArithExpr where
    
    

calc (Number a) = Number a
calc (Frac a b) = simplify (Frac a b)
calc (Var (a, x)) = reduce (Var (a, x))
calc (Mult (Mult (Var (a, x)) (Var (b,y))) ) = 
calc (Mult (Var (a, x)) (Var (b, y)))
    | a == b = Var (a, x + y)
    | otherwise = Mult (calc $ Var (a, x)) (calc $ Var (b, y))
calc (Mult g (Var (a, x))) = Mult (calc g) (Var (a, x))
calc (Mult (Var (a, x)) g) = Mult (Var (a, x)) (calc g)
calc (Plus (Mult (Var (a, x)) g) (Mult (Var (b, y)) f))
    | (a == b && x == y) = Mult (calc $ Plus g f) (Var (a, x))
    | otherwise = (Plus (Mult (Var (a, x)) g) (Mult (Var (b, y)) f))
calc (Plus (Mult g (Var (a, x))) (Mult f (Var (b, y))))
    | (a == b && x == y) = Mult (calc $ Plus g f) (Var (a, x))
    | otherwise = (Plus (Mult g (Var (a, x))) (Mult f (Var (b, y))))
calc (Plus (Frac (Number a) (Number b)) (Frac (Number c) (Number d))) = calc (Frac (Number (a * d + b * c)) (Number (b * d)))
calc (Plus (Number a) (Number b)) = Number (a+b)
calc (Plus a b) = calc (Plus (calc a) (calc b))
calc (Mult (Frac (Number a) (Number b)) (Frac (Number c) (Number d))) = calc (Frac (Number (a * c)) (Number (b * d)))
calc (Mult (Number a) (Number b)) = Number (a * b)
calc (Mult a b) = calc (Mult (calc a) (calc b))
simplify (Frac (Number a) (Number b))
    | (a == b) = Number 1
    | (a == (-1 * b)) = Number (-1)
    | (b > 0 && g /= b) = Frac (Number (a `div` g)) (Number (b `div` g))
    | (b < 0 && g /= b) = Frac (Number ((-1) * a `div` g)) (Number ((-1) * b `div` g))
    | otherwise = Number (a `div` g)
      where g = gcd a b

reduce a = a
removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

makeExpr :: [Char] -> VRArithExpr
makeExpr strs
    | '+' `elem` strs = Plus (makeExpr (fst (tuplized '+'))) (makeExpr (tail (snd (tuplized '+'))))
    | '*' `elem` strs = Mult (makeExpr (fst (tuplized '*'))) (makeExpr (tail (snd (tuplized '*'))))
    | '/' `elem` strs = Frac (makeExpr (fst (tuplized '/'))) (makeExpr (tail (snd (tuplized '/'))))
    | '^' `elem` strs = Var (fst (tuplized '^'), read (tail (snd (tuplized '^'))))
    | all isLetter strs = Var (strs, 1)
    | otherwise = Number (read strs)
        where tuplized f = break (== f) strs

-- testAdd = (calc (makeExpr (individualize (removeSpaces "1/3 + 1/5"))) == Frac (Number 8) (Number 15))
-- testMult = (calc (makeExpr (individualize (removeSpaces "1/3 * -1/5")))) == Frac (Number (-1)) (Number 15)
-- test = (calc (makeExpr (individualize (removeSpaces "1/9 + -7/-8 * -51/33")))) == Frac (Number (983)) (Number (-792))
doTheWholeThing inp = calc (makeExpr (individualize (removeSpaces ((inp))))))