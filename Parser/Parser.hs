module Parser where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser { runParser :: String -> [(a,String)] }

{- primitive parsers -}

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
    [] -> []
    a:as -> [(a,as) | f a]

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string str = Parser $ \s -> [(t,u) | let (t,u) = splitAt (length str) s, str == t]

{- instance definitions for Parser -}

instance Functor Parser where
    fmap f p = Parser $ \s -> [(f a,t) | (a,t) <- runParser p s]
    
instance Applicative Parser where
    pure a = Parser $ \s -> [(a,s)]
    af <*> aa = Parser $ \s -> [(f a,u) | (f,t) <- runParser af s, (a,u) <- runParser aa t]

instance Alternative Parser where
    empty = Parser $ \s -> []
    p1 <|> p2 = Parser $ \s -> runParser p1 s ++ runParser p2 s
    
instance Monad Parser where
    return = pure
    ma >>= f = Parser $ \s -> [(b,u) | (a,t) <- runParser ma s, (b,u) <- runParser (f a) t]

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

(<++) p1 p2 = Parser $ \s ->
    if (runParser p1 s == []) then runParser p2 s
    else runParser p1 s
