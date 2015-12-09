import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser s = Parser { runParser :: String -> [(s,String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    [] -> []
    a:as -> [(a,as) | p a]

instance Functor Parser where
    fmap f p = Parser $ \s -> [(f a,t) | (a,t) <- runParser p s]


char :: Char -> Parser Char
char c = satisfy (c==)

string :: String -> Parser String
string str = Parser $ \s -> [(t,u) | let (t,u) = splitAt (length str) s, str == t]

alpha, digit, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
space = satisfy isSpace

parseTrue, parseFalse :: Parser Bool
parseTrue = token "True" True
parseFalse = token "False" False


token :: String -> a -> Parser a
token s a = fmap (const a) (string s)

instance Applicative Parser where
    pure a = Parser $ \s -> [(a,s)]
    af <*> aa = Parser $ \s -> [(f a,u) | (f,t) <- runParser af s, (a,u) <- runParser aa t]

instance Alternative Parser where
    empty = Parser $ \s -> []
    p1 <|> p2 = Parser $ \s -> runParser p1 s ++ runParser p2 s


parseBool :: Parser Bool
parseBool = fmap read (string "True" <|> string "False")

data IntV = IntV Int deriving (Show)

instance Read IntV where
    readsPrec _ = runParser parseIntV


parseInt = fmap read (some digit)
skipSpaces = fmap (const ()) (many space)
parseIntV = liftA3 (\n s i -> IntV i) (string "IntV") skipSpaces parseInt

parseWith :: Parser a -> String -> a
parseWith p s = case [a | (a,t) <- runParser p s, all isSpace t] of
    [] -> error "no parse"
    [a] -> a
    _ -> error "ambiguous parse"


data ComplexInt = ComplexInt Int Int
    deriving (Show)

--instance Read ComplexInt where
--    readsPrec _ = runParser parseComplex

parseComplexInt = 

