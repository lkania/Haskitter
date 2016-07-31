module Users where

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

------------------------------------------------------------------------------

data User = User { uid :: Int, email :: String, name :: String, password :: String }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToJSON User where
  toJSON(User uid email name password) =
    object ["name" Data.Aeson..= name, "id" Data.Aeson..= uid]

-- name is a reserved keyword in postgreSQL, hence it must be escaped in order to do a quer
getUsers :: AppHandler [User]
getUsers = with pg $ query_ "SELECT id,email,\"name\",password FROM users"

getUserById :: String -> AppHandler (Maybe User)
getUserById user_id = getUserByFunction $ (\user -> show (uid user) == user_id)

getUserByEmail :: String -> AppHandler (Maybe User)
getUserByEmail user_email = getUserByFunction $ (\user -> email user == user_email)

getUserByFunction :: (User -> Bool) -> AppHandler (Maybe User)
getUserByFunction f = do
  users <- getUsers
  return $ if length (filter f users) /= 0 then Just $ head (filter f users) else Nothing

checkPassword :: User -> String -> Bool
checkPassword user user_password = password user == user_password

subscribe :: User -> User -> AppHandler ()
subscribe follower followed = do
  with pg $ execute "INSERT INTO relationships (follower_id,followed_id) VALUES (?,?)" (uid follower,uid followed)
  return ()

signUp :: String -> String -> String -> AppHandler ()
signUp user_email user_name user_password = do
  with pg $ execute "INSERT INTO users (email,name,password) VALUES (?,?,?)" (user_email,user_name,user_password)
  return () 