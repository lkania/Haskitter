module Utils where

------------------------------------------------------------------------------

------------------------------------------------------------------------------
import Application

------------------------------------------------------------------------------

concatAppHandler :: AppHandler [a] -> AppHandler [a] -> AppHandler [a]
concatAppHandler m1 m2 = m1 >>= (\a -> m2 >>= (\b -> return $ a ++ b ))

concatAppHandlerList :: [AppHandler [a]] -> AppHandler [a]
concatAppHandlerList = foldr (\x xs -> concatAppHandler x xs) (return [])