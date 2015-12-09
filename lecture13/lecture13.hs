import Control.Applicative

liftAN  :: Applicative f => ([a] -> b) -> f [a] -> f b
liftAN f a = f <$> a

altconcat :: Alternative f => [f a] -> f a
altconcat cs = foldr (<|>) empty cs