data Complex = Complex Double Double
    deriving (Show,Eq)
    
instance Num Complex where
    Complex a b + Complex c d = Complex (a+c) (b+d)
    Complex a b * Complex c d = Complex (a*c - b*d) (a*d + b*c)
    abs (Complex a b) = Complex (sqrt (a**2 + b**2)) 0
    signum (Complex 0 0) = Complex 0 0
    signum (Complex a b) = Complex (a / (getA $ abs (Complex a b)))
                                   (b / (getA $ abs (Complex a b)))
        where getA (Complex x y) = x
    fromInteger i = Complex (fromInteger i) (fromInteger 0)
    
-- type of map alone
map :: (a -> b) -> [a] -> [b]
-- type of map in map (`replicate` '*')
map (`replicate` '*') :: [Int] -> [[Char]]
-- type of map in map (`replicate` '*') [1,2,3]
map (`replicate` '*') [1,2,3] :: [[Char]]