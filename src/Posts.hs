module Posts where

------------------------------------------------------------------------------
import Control.Applicative
import Snap.Core
import Control.Lens
import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Data.Aeson
import GHC.Int
import Data.Time.LocalTime

------------------------------------------------------------------------------
import Application
import Feed
import Utils

------------------------------------------------------------------------------

data Post = Post { message  ::  String , user_id :: Int} 

instance FromRow Post where
  fromRow = Post <$> field <*> field

instance ToJSON Post where
  toJSON (Post message user_id) =
    object ["message" Data.Aeson..= message]

------------------------------------------------------------------------------
-- | Reading operations

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

------------------------------------------------------------------------------
-- | Writing operations

post :: String -> Int -> LocalTime -> AppHandler GHC.Int.Int64
post message user_id timeStamp= with pg $ execute "INSERT INTO post (message,user_id,created_at) VALUES (?,?,?)" (message,user_id,timeStamp)