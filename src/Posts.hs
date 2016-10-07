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
import Users
import Errors

------------------------------------------------------------------------------

data Post = Post { message :: String , user_id :: Int }
data PostWithUser = PostWithUser { user_post :: Post, user :: User }

instance FromRow Post where
  fromRow = Post <$> field <*> field

instance ToJSON Post where
  toJSON (Post message user_id) =
    object ["message" Data.Aeson..= message,
    		    "user" Data.Aeson..=user_id]

instance ToJSON PostWithUser where
  toJSON (PostWithUser post user) =
    object ["post" Data.Aeson..=post,
            "user" Data.Aeson..=user]

------------------------------------------------------------------------------
-- | Reading operations

getPosts :: ExceptT Error AppHandler [Post]
getPosts = lift $ with pg $ query_ "SELECT message,user_id FROM posts"

getPostsWithUser :: ExceptT Error AppHandler [PostWithUser]
getPostsWithUser = do
  posts <- getPosts
  concatAppHandlerList $ postsToPostsWithUser posts

postsToPostsWithUser :: [Post] -> [ExceptT Error AppHandler PostWithUser]
postsToPostsWithUser posts = map postToPostWithUser posts

postToPostWithUser :: Post -> ExceptT Error AppHandler PostWithUser
postToPostWithUser post = do
  user <- getUserById $ show $ user_id post
  lift $ return $ createPostWithUser post user

createPostWithUser :: Post -> User -> PostWithUser
createPostWithUser post user = PostWithUser post user

getPostByUserId :: Int -> ExceptT Error AppHandler [Post]
getPostByUserId userId = do
  posts <- getPosts
  lift $ return $ filter (\post -> userId == user_id post) posts

getFollowedPostsByUserId :: User -> ExceptT Error AppHandler [Post]
getFollowedPostsByUserId user = do
  follows <- getFollowedsById user
  concatListAppHandlerList $ map (\follow -> getPostByUserId $ followed_id follow) follows

------------------------------------------------------------------------------
-- | Writing operations

createPost :: String -> User -> ExceptT Error AppHandler Post
createPost message user = do
  lift $ with pg $ execute "INSERT INTO posts (message,user_id) VALUES (?,?)" (message,uid user)
  lift. return $ Post message (uid user)
