import Prelude hiding (length, map, product, filter)

length [] = 0
length (_:cs) = 1 + length cs

sumf f [] = 0
sumf f (c:cs) = f c + sumf f cs

product [] = 1
product (c:cs) = c * product cs

map f [] = []
map f (x:xs) = f x : map f xs

square = (^2)
cube = (^3)

-- sumSquares = sumf square
-- sumCubes = sumf cube

sumSquares = sum . map (^2)
sumCubes = sum . map (^3)

-- sumSquaresOfOdds [] = 0
-- sumSquaresOfOdds (x:xs)
--    | odd x = x^2 + sumSquaresOfOdds xs
--    | otherwise = sumSquaresOfOdds xs

sumSquaresOfOdds = sum . map (^2) . filter odd

filter p [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

allp [] x = True
allp (p:ps) x
    | p x = allp ps x
    | otherwise = False

--allp ps x = not . elem False $ map ps x

divisibleBy d n
    | n `rem` d == 0 = True
    | otherwise = False

filterAll = filter . allp




