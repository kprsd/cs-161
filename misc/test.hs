-- 1.
iterate' :: (a -> a) -> a -> [a]
iterate' f a = (f a) : (iterate' f (f a))

-- 2.
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] c = c
merge' b [] = b
merge' (b:bs) (c:cs)
    | b < c = b : (merge' bs (c:cs))
    | c < b = c : (merge' cs (b:bs))
    | b == c = b : c : (merge' bs cs)

-- 3.
main = do
    inp <- getLine
    let intp = (read inp) :: Int
    mapM_ (putStrLn . show) [1..intp]