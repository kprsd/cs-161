import Prelude hiding (even, odd, rem)
data NaturalNumber = Zero | S NaturalNumber
    deriving (Show)

zero = Zero
one = S zero
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven
nine = S eight
ten = S nine
infinity = S infinity

instance Eq NaturalNumber where
    Zero == Zero = True
    Zero == S _ = False
    S _ == Zero = False
    S x == S y = x == y

instance Ord NaturalNumber where
    compare Zero Zero = EQ
    compare Zero (S _) = LT
    compare (S _) Zero = GT
    compare (S x) (S y) = compare x y
    

instance Num NaturalNumber where
    x + Zero = x
    x + S y = S (x + y)
    
    x * Zero = Zero
    x * S y = x + x * y
    
    x - Zero = x
    Zero - x = Zero
    (S x) - (S y) = x - y
    
    abs x = x
    
    signum Zero = Zero
    signum x = S Zero
    
    fromInteger n
        | n > 0 = S (fromInteger (n-1))
        | n == 0 = Zero

even n
    | n == 0 = True
    | n == 1 = False
    | otherwise = even (n-2)
odd n = not (even n)

rem a b
    | a < b = a
    | otherwise = rem (a-b) b
    
primrec f bc Zero = bc
primrec f bc (S y) = f (primrec f bc y)
plus a b = primrec (S) b a
mult a b = primrec ((plus) b) Zero a

nat :: NaturalNumber -> NaturalNumber
nat = id