import System.IO
import System.Exit
import System.Environment


main = do
    cs <- getContents
    putStrLn $ unlines (zipWith (++) numbs (lines cs))

numbs = zipWith (++) (map show [1..]) (repeat ". ")