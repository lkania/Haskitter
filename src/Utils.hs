module Utils where

------------------------------------------------------------------------------

------------------------------------------------------------------------------
import Application

------------------------------------------------------------------------------

concatListAppHandler :: AppHandler [a] -> AppHandler [a] -> AppHandler [a]
concatListAppHandler m1 m2 = m1 >>= (\a -> m2 >>= (\b -> return $ a ++ b ))

concatListAppHandlerList :: [AppHandler [a]] -> AppHandler [a]
concatListAppHandlerList = foldr (\x xs -> concatListAppHandler x xs) (return [])

concatAppHandler :: Monad m => m a -> m [a] -> m [a]
concatAppHandler m1 m2 = m1 >>= (\x -> m2 >>= (\xs -> return $ x:xs))

concatAppHandlerList :: Monad m => [m a] -> m [a]
concatAppHandlerList = foldr (\x xs -> concatAppHandler x xs) (return [])