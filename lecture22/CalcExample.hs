module Main where

import Calc

square :: Calculation
square = do
    kDup
    kMul

hypotenuse :: Calculation
hypotenuse = do
    square
    kSwap
    square
    kAdd
    kSqrt

test :: Double
test = perform $ do
    kEnter 1
    kEnter 2
    kAdd
    kEnter 3
    kMul    

testStoRcl = perform $ do
    kEnter 3
    kSto
    kEnter 4
    kEnter 5
    kAdd
    kRcl
    kAdd
