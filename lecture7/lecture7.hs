takeUntil               :: (a -> Bool) -> [a] -> [a]
takeUntil _ []          =  []
takeUntil p (x:xs) = if p x then x : takeUntil p xs
                     else if not (p x) then x : []
                     else undefined
cseq :: Integer -> [Integer]
cseq = takeUntil (/= 1) . iterate collatz
collatz :: Integer -> Integer
collatz x = case (odd x) of
             True -> 3*x + 1
             False -> x `div` 2



data NaturalNumber = Zero | S NaturalNumber
    deriving (Show)

eqNat             :: NaturalNumber -> NaturalNumber -> Bool
eqNat Zero  Zero  =  True
eqNat (S n) (S m) =  eqNat n m
eqNat _     _     =  False

eqNat1 :: NaturalNumber -> NaturalNumber -> Bool
eqNat1 = \x y -> case (x,y) of
                 (Zero, Zero) -> True
                 (S x, S y) -> eqNat1 x y
                 otherwise -> False

eqNat2 :: NaturalNumber -> NaturalNumber -> Bool
eqNat2 = \x y -> case x of
                 (Zero) -> case y of
                           (Zero) -> True
                           otherwise -> False
                 (S a) -> case y of
                          (S b) -> eqNat2 a b
                          otherwise -> False

eqNat3 :: NaturalNumber -> NaturalNumber -> Bool
eqNat3 = \x -> case x of
               (Zero) -> \y -> case y of
                               (Zero) -> True
                               otherwise -> False
               (S a) -> \y -> case y of
                               (S b) -> eqNat3 a b
                               otherwise -> False
                     
