import Prelude hiding (product, map)
product = foldr (*) 1
map f = foldr ((:) . f) []