import Control.Applicative
import Control.Monad
import Data.Char
import Parser

data ComplexInt = ComplexInt Int Int
    deriving (Show)

instance Read ComplexInt where
    readsPrec _ = runParser (parseTuples <|> parseReals)

digit :: Parser Char
digit = satisfy isDigit

parseInt :: Parser Int
parseInt = fmap read (some digit)

parseReals :: Parser ComplexInt
parseReals = fmap (\x -> ComplexInt (read x) 0) (some digit)
parseTuples :: Parser ComplexInt
parseTuples = do
    char '('
    x <- parseInt
    char ','
    y <- parseInt
    char ')'
    return (ComplexInt x y)