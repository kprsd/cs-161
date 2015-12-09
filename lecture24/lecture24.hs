import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Applicative
import Data.Char

data Expression = DoubleValue Double
                | Sum Expression Expression
                | Difference Expression Expression
                | Product Expression Expression
                | Quotient Expression Expression
    deriving (Show, Eq)


wchar :: Char -> ReadP Char
wchar c = skipSpaces *> char c
wstring :: String -> ReadP String
wstring s = skipSpaces *> string s
parsePosInt :: ReadP Expression
parsePosInt = do
    whole <- skipSpaces *> munch isDigit
    return $ DoubleValue (read (whole ++ ".0") :: Double)
parseNegInt :: ReadP Expression
parseNegInt = do
    char '-'
    whole <- skipSpaces *> munch isDigit
    return $ DoubleValue (read ("-" ++ whole ++ ".0") :: Double)
parseNegDouble :: ReadP Expression
parseNegDouble = do
    char '-'
    whole <- skipSpaces *> munch isDigit
    char '.'
    frac <- munch isDigit
    return $ DoubleValue (read ("-" ++ whole ++ "." ++ frac) :: Double)
parsePosDouble :: ReadP Expression
parsePosDouble = do
    whole <- skipSpaces *> munch isDigit
    char '.'
    frac <- munch isDigit
    return $ DoubleValue (read (whole ++ "." ++ frac) :: Double)
parseDouble :: ReadP Expression
parseDouble = parseNegDouble <++ parsePosDouble <++ parseNegInt <++ parsePosInt

parseConst :: a -> ReadP b -> ReadP a
parseConst value = fmap (const value)

instance Read Expression where
    readsPrec _ = readP_to_S parseExpr

evalExpr :: Expression -> Double
evalExpr (DoubleValue a) = a
evalExpr (Product a b) = (evalExpr a) * (evalExpr b)
evalExpr (Quotient a b) = (evalExpr a) / (evalExpr b)
evalExpr (Sum a b) = (evalExpr a) + (evalExpr b)
evalExpr (Difference a b) = (evalExpr a) - (evalExpr b)

eval expr = evalExpr (read expr :: Expression)

parseExpr :: ReadP Expression
parseExpr = prec 0 where
    prec 0 = chainl1 (prec 1) (summ +++ difference)
    prec 1 = chainl1 (prec 2) (mult +++ divide)
    prec 2 = parseDouble +++ parseParens (prec 0)
    summ = parseConst Sum $ wchar '+'
    difference = parseConst Difference $ wchar '-'
    mult = parseConst Product $ wchar '*'
    divide = parseConst Quotient $ wchar '/'
    parseParens = between (wchar '(') (wchar ')')
