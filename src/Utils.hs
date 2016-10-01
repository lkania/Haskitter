module Utils where

------------------------------------------------------------------------------

------------------------------------------------------------------------------
import Application

------------------------------------------------------------------------------

concatListAppHandler :: Monad m => m [a] -> m [a] -> m [a]
concatListAppHandler m1 m2 = m1 >>= (\a -> m2 >>= (\b -> return $ a ++ b ))

concatListAppHandlerList :: Monad m => [m [a]] -> m [a]
concatListAppHandlerList = foldr (\x xs -> concatListAppHandler x xs) (return [])

concatAppHandler :: Monad m => m a -> m [a] -> m [a]
concatAppHandler m1 m2 = m1 >>= (\x -> m2 >>= (\xs -> return $ x:xs))

concatAppHandlerList :: Monad m => [m a] -> m [a]
concatAppHandlerList = foldr (\x xs -> concatAppHandler x xs) (return [])