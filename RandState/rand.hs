import UCState
import RandState
import StateExample
import System.Random
import Control.Monad

-- First problem (test)
testStateExample2 :: Int -> IO Bool
testStateExample2 n = (stateExample2 n) >>= (\x -> return (length x == n))

-- Third problem test
testRandR :: (Int, Int) -> IO Bool
testRandR (x, y) = do
    gen <- newStdGen
    let n = runRandom (randR (x, y)) gen
    return (x <= n && y >= n)

-- Third problem
randR :: Random a => (a, a) -> RandState a
randR b = do
    gen <- get
    let (x, gen') = randomR b gen
    put gen'
    return x

-- Fourth problem
rollTwoDice :: RandState Int
rollTwoDice = do
    x <- randR (1, 6)
    y <- randR (1, 6)
    return (x + y)


-- Fourth problem test
-- testRollTwoDice :: State StdGen [Int]
testRollTwoDice = do
    gen <- newStdGen
    let rs = fst $ runState (replicateM 100000 rollTwoDice) gen 
    let a = "For "
    let c = ": "
    let b = " instances"
    let y = [ a ++ show z ++ c ++ show (countNums z rs) ++ b |  z <- [2 .. 12] ]
    putStrLn (unlines y)
    -- putStrLn (map show rs)

countNums :: Int -> [Int] -> Int
countNums n xs = length [ x | x <- xs, x == n ]

-- Data types to represent playing cards
data CardValue = King | Queen | Jack | NumberCard Int
    deriving (Show, Eq)
data CardSuit = Hearts | Diamonds | Spades | Clubs
    deriving (Show, Eq)
data PlayingCard = PlayingCard CardSuit CardValue
    deriving (Show, Eq)

{-
 - fullCardDeck will be a deck of cards, 52 in total, with a King, a Queen, 
 - a Jack and NumberCards from 1 to 10 for each suit.
 -}
fullCardDeck :: [PlayingCard]
fullCardDeck = [ PlayingCard s v | s <- allsuits, v <- allvals ] where
        allvals = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allsuits = [Hearts, Diamonds, Spades, Clubs]

-- removeCard :: [PlayingCard] -> IO (PlayingCard, [PlayingCard])
removeCard :: [PlayingCard] -> IO [PlayingCard]
removeCard cds = do
    gen <- newStdGen
    let n = runRandom (randR (0, length cds - 1)) gen
    return ([(cds !! n)] ++ [ c | c <- cds, c /= (cds !! n) ])

shuffleDeck :: [PlayingCard] -> IO [PlayingCard]
shuffleDeck [] = do return []
shuffleDeck h = do
    (g:gs) <- removeCard h
    y <- shuffleDeck gs
    return (g:y)

shuffleADeck :: IO [PlayingCard]
shuffleADeck = shuffleDeck (fullCardDeck)

testShuffleADeck :: IO Bool
testShuffleADeck = shuffleADeck >>= (return . (\x -> length x == 52))

-- Second problem (test)
testAll :: IO ()
testAll = do
    tse2 <- testStateExample2 3
    trr <- testRandR (0, 15)
    tsad <- testShuffleADeck
    if tse2 then putStrLn "testStateExample2 succeeded."
    else putStrLn "testStateExample2 failed."
    if trr then putStrLn "testRandR succeeded."
    else putStrLn "testRandR failed."
    if tsad then putStrLn "testShuffleADeck succeeded."
    else putStrLn "testShuffleADeck failed."
    putStrLn "testRollTwoDice results: "
    testRollTwoDice