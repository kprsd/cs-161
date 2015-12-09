import Control.Monad hiding (join)

join :: Monad m => m (m a) -> m a
join y = y >>= (\x -> x)