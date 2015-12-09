import System.Random

threeCoins :: StdGen -> (Int, Int, Int)
threeCoins gen =
    let (firstCoin, newGen) = randomR (1, 10) gen
        (secondCoin, newGen') = randomR (1, 10) newGen
        (thirdCoin, newGen'') = randomR (1, 10)  newGen'
    in  (firstCoin, secondCoin, thirdCoin)
