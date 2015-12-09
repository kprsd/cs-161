data Poly = Poly [Double]
    deriving (Show, Eq)

evalPoly :: Poly -> Double -> Double
evalPoly (Poly x) n = sum (zipWith (*) x (terms n))
                        where terms n = [ n^y | y <- [0 .. (length x - 1)]]