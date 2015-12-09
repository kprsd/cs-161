import Text.ParserCombinators.ReadP as R
import Data.Char

data ComplexInt = ComplexInt Int Int
    deriving (Show)

parseInt :: ReadP Int
parseInt = do
    posNeg <- (string "-") <++ (string "")
    num <- munch isDigit
    return $ read $ posNeg ++ num

parseReals :: ReadP ComplexInt
parseReals = do
    num <- parseInt
    return (ComplexInt num 0)

parseTuple :: ReadP ComplexInt
parseTuple = do
    char '('
    r <- parseInt
    char ','
    i <- parseInt
    char ')'
    return (ComplexInt r i)

instance Read ComplexInt where
    readsPrec _ = readP_to_S (parseTuple +++ parseReals)