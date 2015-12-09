import Control.Category hiding ((.))
import System.IO
import System.Exit
import System.Environment


main = do
    cs <- getContents
    putStrLn $ unlines (zipWith (++) (spaceNumbs (lines cs)) (lines cs))

numbs = zipWith (++) (map show [1..]) (repeat ". ")
spaceNumbs eachN = map (spaceOut eachN) numbs
spaceOut ean nms
    | nms !! ean 