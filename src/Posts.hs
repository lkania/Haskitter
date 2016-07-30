module Posts where

------------------------------------------------------------------------------
import Control.Applicative
import Snap.Core
import Control.Lens
import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Data.Aeson

------------------------------------------------------------------------------
import Application
import Feed

------------------------------------------------------------------------------

data Post = Post { message  ::  String , user_id :: Int} 

instance FromRow Post where
  fromRow = Post <$> field <*> field

instance ToJSON Post where
  toJSON (Post message user_id) =
    object ["message" Data.Aeson..= message]

getPosts :: AppHandler [Post]
getPosts = with pg $ query_ "SELECT message,user_id FROM posts"

getPostByUserId :: Int -> AppHandler [Post]
getPostByUserId userId = do
  posts <- getPosts
  return $ filter (\post -> userId == user_id post) posts

getFollowedPostsByUserId :: String -> AppHandler [Post]
getFollowedPostsByUserId userId = do
  follows <- getFollowedsById userId
  concatAppHandlerList $ map (\follow -> getPostByUserId $ followed_id follow) follows

concatAppHandler :: AppHandler [a] -> AppHandler [a] -> AppHandler [a]
concatAppHandler m1 m2 = m1 >>= (\a -> m2 >>= (\b -> return $ a ++ b ))

concatAppHandlerList :: [AppHandler [a]] -> AppHandler [a]
concatAppHandlerList = foldr (\x xs -> concatAppHandler x xs) (return [])