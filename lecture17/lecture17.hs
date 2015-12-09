-- 1.
ma >>= g = State $ \s ->
    let (a, t) = runState ma s
        mb = g a
        (b, u) = runState mb t
    in (b, u)
-- 2.
ma >>= g = State $ \s ->
    let (a, t) = runState ma s
    in runState (g a) t
-- 3.
ma >>= g = State $ \s ->
           ((\(a, t) -> runState (g a) t) . (runState ma s))
-- 4.
ma >>= g = State $ (\(a, t) -> runState (g a) s) . runState ma

-- 1.
return = pure
-- 2.
return a = State $ \s -> (a, s)
-- 3.
return a = State $ (,) a