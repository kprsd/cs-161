import System.IO
import Data.List
import Data.Char
import Data.Ord
import System.Exit
import System.Environment



checkRealWords :: [String] -> [String] -> Int
checkRealWords wds cds = length (wds `intersect` cds)

rot n str = map (rotChar n) str where
    rotChar n c
        | isLower c = rotCase 'a' n c
        | isUpper c = rotCase 'A' n c
        | otherwise = c
    rotCase cas n c = chr (ord cas + (ord c - ord cas + n)  `mod` 26)

mainloop inh list =
    do
    ineof <- hIsEOF inh
    if ineof
        then return list
        else do
            inpStr <- hGetLine inh
            let list' = inpStr:list
            mainloop inh list'

decode :: [String] -> [String] -> Int -> [String]
decode dic wds n = dic `intersect` (map (rot n) wds)

genDecodes :: [String] -> [String] -> Int -> [[String]]
genDecodes dic wds 26 = []
genDecodes dic wds n = ((decode dic wds n):(genDecodes dic wds (n+1)))

lgenDecodes :: [String] -> [String] -> Int -> [Int]
lgenDecodes dic wds 26 = []
lgenDecodes dic wds n = ((length $ decode dic wds n):(lgenDecodes dic wds (n+1)))

pickRight :: [String] -> [String] -> Int -> (Int, [String])
pickRight dic wds 26 = ((-1), [])
pickRight dic wds n
    | length (decode dic wds n) == maximum (lgenDecodes dic wds n) = (n, (decode dic wds n))
    | otherwise = pickRight dic wds (n+1)

main :: IO ()
main = do
    [args] <- getArgs
    tw <- readFile args
    inh <- openFile "words" ReadWriteMode
    wordDict <- mainloop inh []
    let x = pickRight wordDict (words tw) 0
    putStrLn("n=" ++ show (fst x))
    putStrLn(unwords (snd x))
