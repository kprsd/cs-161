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

rotvStdin :: String -> IO ()
rotvStdin = interact . rotv

rotdStdin :: String -> IO ()
rotdStdin = interact . rotd

-- Decrypt Vigenere
rotd :: String -> String -> String
rotd k m = zipWith (addLetters) m (cycle (tail k)) where
    addLetters ml kl
        | isLower ml = rotCase 'a' ml kl
        | isUpper ml = rotCase 'A' ml (toUpper kl)
        | otherwise = ml
    rotCase cas a b = chr (ord cas + (ord a - ord b) `mod` 26)

-- Encrypt Vigenere
rotv :: String -> String -> String
rotv k m = zipWith (addLetters) m (cycle k) where  
    addLetters ml kl
        | isLower ml = rotCase 'a' ml kl
        | isUpper ml = rotCase 'A' (toUpper ml) kl
        | otherwise = ml
    rotCase cas a b = chr (ord cas + (ord a + ord b - ord cas - ord cas)  `mod` 26)

    
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
            | x =~ "^[a-z]+$" -> rotvStdin x
            | x =~ "^-[a-z]+$" -> rotdStdin x
            | x =~ "^-?[0-9]+$" -> rotStdin (read x)
            | otherwise         -> usage
        _   -> usage