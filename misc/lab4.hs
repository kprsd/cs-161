import System.IO
import Data.Maybe
import Data.Char
import Data.List

data ArithExpr = Number Int | Plus ArithExpr ArithExpr | Mult ArithExpr ArithExpr
    deriving (Show, Eq)

calc :: ArithExpr -> ArithExpr

calc a = Number (intCalc a)

intCalc (Number x) = x
intCalc (Plus a b) = case (a, b) of
                    (Number x, Number y) -> x + y
                    (Number x, s) -> x + intCalc s
                    (f, Number y) -> intCalc f + y
                    (f, s) -> intCalc f + intCalc s
intCalc (Mult a b) = case (a, b) of
                       (Number x, Number y) -> x * y
                       (Number x, s) -> x * intCalc s
                       (f, Number y) -> y * intCalc f
                       (f, s) -> intCalc f + intCalc s

removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

individualize :: String -> [String]
individualize [] = []
individualize inStr
    | isDigit (head inStr) = (takeWhile isDigit inStr) : (individualize (dropWhile isDigit inStr))
    | head inStr == '-' = ("-" ++ takeWhile isDigit (tail inStr)) : (individualize (dropWhile isDigit (tail inStr)))
    | otherwise = [head inStr] : (individualize (tail inStr))

makeExpr :: [String] -> ArithExpr
makeExpr strs
    | "+" `elem` strs = Plus (makeExpr (fst (tuplized "+"))) (makeExpr (tail (snd (tuplized "+"))))
    | "*" `elem` strs = Mult (makeExpr (fst (tuplized "*"))) (makeExpr (tail (snd (tuplized "*"))))
    | otherwise = Number $ read . head $ strs
        where tuplized f = break (== f) strs

main :: IO ()
main = do
    inp <- getLine
    print (calc (makeExpr (individualize (removeSpaces (read (inp))))))