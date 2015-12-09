type Person = String

father :: Person -> Maybe Person
father p = case father p of
    Nothing -> Nothing
    Just fp -> fp

instance Monad Maybe where
    return x = Just x
    Just x  >>= f = f x
    Nothing >>= _ = Nothing

--return a >>= f
--= Maybe a >>= f
--= f a

--= Nothing >>= f
--= Nothing


--Maybe a >>= return
--= return a
--= Maybe a

--Nothing >>= return
--= Nothing


--(Maybe a >>= f) >>= g
--= f a >>= g
--= (\x -> f x >>= g) a
--= Just a >>= (\x -> f x >>= g)

-- (Nothing >>= f) >>= g
--= Nothing >>= g
--= Nothing
--= Nothing >>= _
--= Nothing >>= (\x -> f x >>= g)

Either s does produce an explicit fail value that can be translated to mzero. Unlike Maybe, which produces Nothing when it fails, Either s produces upon failure the value that it is given (this occurs when it is the Left value instead of Right). Information would be lost if Either s were used in MonadPlus.
