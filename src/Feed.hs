module Feed where

------------------------------------------------------------------------------
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
import Application
import Users
import Errors

------------------------------------------------------------------------------

getFollows :: ExceptT Error AppHandler [Follow]
getFollows = lift $ with pg $ query_ "SELECT follower_id,followed_id FROM relationships"

getFollowedsById :: User -> ExceptT Error AppHandler [Follow]
getFollowedsById user = do
  follows <- getFollows
  (lift . return) $ filter (\follow -> (follower_id follow) == (uid user)) follows
