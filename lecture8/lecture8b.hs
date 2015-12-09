module Main where

import System.Environment
import Data.Char

msg :: String -> String
msg user = "Hello, " ++ [toUpper (head user)] ++ tail user ++ "!"

main :: IO ()
main = do
    user <- getEnv "USER"
    putStrLn $ msg user
    