import Data.Complex
import GHC.Integer.Logarithms

data Poly a = Poly [Complex Double]
    deriving (Show)


-- Use Horner's scheme for evaluation
polyEval :: Poly a -> (Complex Double) -> (Complex Double)
polyEval (Poly [])  _ = 0
polyEval (Poly (c:cs)) x = c + x * polyEval (Poly cs) x
xtest = Poly [1,2,1]
test2 = polyEval xtest 1 == 4

polyAdd :: Poly a -> Poly b -> Poly c
polyAdd x y = Poly (helpPolyAdd x y)
              where helpPolyAdd (Poly bs) (Poly []) = bs
                    helpPolyAdd (Poly []) (Poly bs) = bs
                    helpPolyAdd (Poly (b:bs)) (Poly (c:cs)) = ((b+c) : helpPolyAdd (Poly bs) (Poly cs))
ytest = Poly [0, 1]
ztest = Poly [0, 0, 1]
test3 = polyEval (polyAdd ytest ztest) 1 == 2

polyScalMult :: Poly a -> Complex Double -> Poly b
polyScalMult cs s = Poly (helpPSMult cs s)
                    where helpPSMult (Poly cs) s = map (* s) cs
test4a = polyEval (polyScalMult ytest 2) 1 == 2

polyNegate :: Poly a -> Poly b
polyNegate cs = polyScalMult cs (-1)
test4b = polyEval (polyNegate ytest) 1 == -1

polyDiff :: Poly a -> Poly b -> Poly c
polyDiff a b = polyAdd a (polyNegate b)
test4c = polyEval (polyDiff xtest ytest) 1 == 3

polyAbs :: Poly a -> Poly b
polyAbs cs = Poly (helpPAbs cs)
             where helpPAbs (Poly cs) = map (abs) cs
test4d = polyEval (polyAbs (Poly [1, -1, 1])) 1 == 3

polyApproxEqual a b eps
    | gtrl cd <= eps = True
    | otherwise = False
    where
        gtrl (g:+h) = g
        cd = sum $ polyCoeffs $ polyAbs $ (polyDiff a b)
test5 = polyApproxEqual (Poly [1,1]) (Poly [2,1]) 2

-- Collaborated with Apoorva Shah for polyMult
polyMult :: Poly a -> Poly b -> Poly c
polyMult (Poly []) b = Poly []
polyMult (Poly (xi:xs)) b = polyAdd (polyScalMult b xi) (Poly (0:(polyCoeffs(polyMult (Poly xs) b))))
test6 = polyEval (polyMult ytest ztest) 1 == 1

data PolyPoints x y degree = PolyPoints [Complex Double] [Complex Double] Int
    deriving (Show)

toPolyPoints :: Poly t -> [Complex Double] -> PolyPoints x y degree
toPolyPoints (Poly pf) [] = undefined
toPolyPoints (Poly pf) xs = PolyPoints xs (map (polyEval (Poly pf)) xs) (length pf - 1)

tests = test1 && test2 && test3 && test4a && test4b && test4c && test4d && test5 && test6 && testFFTandInvFFT && testPMFFT

-- Collaborated with Bowen Wang for polyPointsAdd
polyPointsAdd :: PolyPoints a b c -> PolyPoints d e f -> PolyPoints x y degree
polyPointsAdd (PolyPoints ax ay ad) (PolyPoints bx by bd)
    | ax == bx = PolyPoints ax (zipWith (+) ay by) (max ad bd)
    | otherwise = undefined
polyPointsScalMult :: PolyPoints a b c -> Complex Double -> PolyPoints x y degree
polyPointsScalMult (PolyPoints x y d) s = PolyPoints x (map (*s) y) d
polyPointsMult :: PolyPoints a b c -> PolyPoints d e f -> PolyPoints x y degree
polyPointsMult (PolyPoints ax ay ad) (PolyPoints bx by bd)
    | ax == bx = PolyPoints ax (zipWith (*) ay by) (ad + bd)
    | otherwise = undefined
data PolyPointsFFT x y degree = PolyPointsFFT [Complex Double] [Complex Double] Int
    deriving (Show)

polyEvens (Poly (x)) = Poly (map (x !!) [0, 2 .. length x - 1])
polyOdds (Poly (x)) = Poly (map (x !!) [1, 3 .. length x - 1])
polyPad (Poly (x))
    | (length x) `elem` [ 2^n | n <- [0 .. length x] ] = (Poly (x))
    | otherwise = polyPad (Poly (x ++ [0]))

getRootOfUnity n j = cos (exprsn) :+ sin (exprsn)
    where exprsn = ((-2) * pi * j) / n

getRootsOfUnity :: Double -> [Complex Double]
getRootsOfUnity n = [ getRootOfUnity n x | x <- [0 .. n - 1] ]



fftEval (Poly []) _ = 0
fftEval (Poly a) x
    | length a == 1 = a !! 0
    | otherwise = (fftEval (polyEvens (Poly a)) (x**2)) + x * (fftEval (polyOdds (Poly a)) (x**2))

deg (Poly xs) = fromIntegral (length xs)

toPolyPointsFFT :: (Poly a) -> (PolyPointsFFT a a b)
toPolyPointsFFT a = (PolyPointsFFT roots (map (fftEval (a)) (roots)) (fromIntegral $ deg a))
    where roots = getRootsOfUnity (deg a)

polyPointsList a = map (fftEval (a)) (roots)
    where roots = getRootsOfUnity (deg a)

fft = toPolyPointsFFT

-- Collaborated with Dan Rubenstein for invfft
invfft (PolyPointsFFT x y degree) = Poly (map (/ (fromIntegral (length x))) (map conjugate (polyPointsList (Poly (map conjugate y)))))

testFFTandInvFFT = polyApproxEqual (invfft $ fft (Poly [1, 3, 3, 1])) (Poly [1, 3, 3, 1]) 0.0000000001

fromPolyPointsFFT = invfft

polyMultFFT (Poly a) (Poly b) = fromPolyPointsFFT (polyPointsMultFFT aPts bPts)
    where aPts = PolyPointsFFT roots (map (fftEval (polyPad (Poly a))) (roots)) (fromIntegral $ deg $ polyPad (Poly a))
          bPts = PolyPointsFFT roots (map (fftEval (polyPad (Poly b))) (roots)) (fromIntegral $ deg $ polyPad (Poly a))
          roots = getRootsOfUnity (fromIntegral (length a + length b) - 1)

polyPointsMultFFT (PolyPointsFFT ax ay ad) (PolyPointsFFT bx by bd)
    | ax == bx = PolyPointsFFT ax (zipWith (*) ay by) (ad + bd)
    | otherwise = undefined
    
testPMFFT = polyApproxEqual (Poly [1, 2, 3, 2, 1]) (polyMultFFT (Poly [1, 1, 1]) (Poly [1, 1, 1])) 0.00000000000001

polyCoeffs :: Poly t -> [Complex Double]
polyCoeffs (Poly c) = c
test1 = polyEval (Poly (polyCoeffs xtest)) 1 == polyEval xtest 1