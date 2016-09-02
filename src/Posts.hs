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

getPosts :: AppHandler [Post]
getPosts = with pg $ query_ "SELECT message,user_id FROM posts"

getPostsWithUser :: AppHandler [PostWithUser]
getPostsWithUser = do
  posts <- getPosts
  concatAppHandlerList $ postsToPostsWithUser posts

postsToPostsWithUser :: [Post] -> [AppHandler PostWithUser]
postsToPostsWithUser posts = map postToPostWithUser posts

postToPostWithUser :: Post -> AppHandler PostWithUser
postToPostWithUser post = do
  user <- maybeUserToUserAppHandler $ getUserById $ show $ user_id post
  return $ createPostWithUser post user

createPostWithUser :: Post -> User -> PostWithUser
createPostWithUser post user = PostWithUser post user

maybeUserToUserAppHandler :: AppHandler (Maybe User) -> AppHandler User
maybeUserToUserAppHandler app_maybe_user = app_maybe_user >>= (\maybe_user -> return $ justUser maybe_user)  

justUser :: Maybe User -> User
justUser (Just user) = user 

getPostByUserId :: Int -> AppHandler [Post]
getPostByUserId userId = do
  posts <- getPosts
  return $ filter (\post -> userId == user_id post) posts

getFollowedPostsByUserId :: String -> AppHandler [Post]
getFollowedPostsByUserId userId = do
  follows <- getFollowedsById userId
  concatListAppHandlerList $ map (\follow -> getPostByUserId $ followed_id follow) follows

------------------------------------------------------------------------------
-- | Writing operations 

createPost :: String -> User -> AppHandler ()
createPost message user = do
  with pg $ execute "INSERT INTO posts (message,user_id) VALUES (?,?)" (message,uid user)
  return ()