module Main where
    
import Data.Char
import System.Exit
import System.Environment
import System.IO
import Text.Regex.Posix

rot :: Int -> String -> String
rot n str = map (rotChar n) str where
    rotChar n c
        | isLower c = rotCase 'a' n c
        | isUpper c = rotCase 'A' n c
        | otherwise = c
    rotCase cas n c = chr (ord cas + (ord c - ord cas + n)  `mod` 26)

rotStdin :: Int -> IO ()
rotStdin = interact . rot


usage :: IO ()
usage = do
    progname <- getProgName
    hPutStrLn stderr $ "usage: " ++ progname ++ " [n]"
    exitWith $ ExitFailure 255

main :: IO ()
main = do
    args <- getArgs
    case args of
        []  -> rotStdin 13
        [x]
            | x =~ "^-?[0-9]+$" -> rotStdin (read x)
            | otherwise         -> usage
        _   -> usage