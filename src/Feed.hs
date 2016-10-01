module Feed where

------------------------------------------------------------------------------
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
import Application
import Users
import Errors

------------------------------------------------------------------------------
data Follow = Follow {follower_id :: Int, followed_id :: Int}

instance FromRow Follow where
  fromRow = Follow <$> field <*> field

getFollows :: ExceptT Error AppHandler [Follow]
getFollows = lift $ with pg $ query_ "SELECT follower_id,followed_id FROM relationships" 

getFollowedsById :: String -> ExceptT Error AppHandler [Follow]
getFollowedsById user_id = do 
  follows <- getFollows
  (lift . return) $ filter (\follow -> (show $ follower_id follow) == user_id) follows


