import Control.Monad

slkdfja xs ys = do
    x <- xs
    y <- ys
    guard (x == y)
    return (x + y)