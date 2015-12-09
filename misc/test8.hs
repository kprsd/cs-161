sumOfTwoSetsOfCubes n = [ (a,b,c,d) |
    a <- [1..n],
    b <- [1..n],
    c <- [1..n],
    d <- [1..n],
    a^3 + b^3 == c^3 + d^3,
    a /= c,
    a /= d]