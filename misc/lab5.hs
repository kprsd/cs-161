import System.IO
import Data.Maybe
import Data.Char
import Data.List

data RArithExpr = Number Int | Frac RArithExpr RArithExpr | Plus RArithExpr RArithExpr | Mult RArithExpr RArithExpr
    deriving (Show, Eq)

calc :: RArithExpr -> RArithExpr

calc (Number a) = Number a
calc (Frac a b) = simplify (Frac a b)
calc (Plus (Frac (Number a) (Number b)) (Frac (Number c) (Number d))) = calc (Frac (Number (a * d + b * c)) (Number (b * d)))
calc (Plus (Number a) (Number b)) = Number (a+b)
calc (Plus a b) = calc (Plus (calc a) (calc b))
calc (Mult (Frac (Number a) (Number b)) (Frac (Number c) (Number d))) = calc (Frac (Number (a * c)) (Number (b * d)))
calc (Mult (Number a) (Number b)) = Number (a * b)
calc (Mult a b) = calc (Mult (calc a) (calc b))

simplify (Frac (Number a) (Number b))
    | (b > 0 && g /= b) = Frac (Number (a `div` g)) (Number (b `div` g))
    | (b < 0 && g /= b) = Frac (Number ((-1) * a `div` g)) (Number ((-1) * b `div` g))
    | otherwise = Number (a `div` g)
      where g = gcd a b


gcd' a b = case (a, b) of
             (0, 0) -> error "Divide by zero"
             (a, 0) -> abs a
             (a, b) -> gcd' b (a `rem` b)


removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

individualize :: String -> [String]
individualize [] = []
individualize inStr
    | isDigit (head inStr) = (takeWhile isDigit inStr) : (individualize (dropWhile isDigit inStr))
    | head inStr == '-' = ("-" ++ takeWhile isDigit (tail inStr)) : (individualize (dropWhile isDigit (tail inStr)))
    | otherwise = [head inStr] : (individualize (tail inStr))

makeExpr :: [String] -> RArithExpr
makeExpr strs
    | "+" `elem` strs = Plus (makeExpr (fst (tuplized "+"))) (makeExpr (tail (snd (tuplized "+"))))
    | "*" `elem` strs = Mult (makeExpr (fst (tuplized "*"))) (makeExpr (tail (snd (tuplized "*"))))
    | "/" `elem` strs = Frac (makeExpr (fst (tuplized "/"))) (makeExpr (tail (snd (tuplized "/"))))
    | otherwise = Number $ read . head $ strs
        where tuplized f = break (== f) strs
tests = test && testAdd && testMult
testAdd = (calc (makeExpr (individualize (removeSpaces "1/3 + 1/5"))) == Frac (Number 8) (Number 15))
testMult = (calc (makeExpr (individualize (removeSpaces "1/3 * -1/5")))) == Frac (Number (-1)) (Number 15)
test = (calc (makeExpr (individualize (removeSpaces "1/9 + -7/-8 * -51/33")))) == Frac (Number (-983)) (Number (792))
main :: IO ()
main = do
    inp <- getLine
    print (calc (makeExpr (individualize (removeSpaces ((inp))))))