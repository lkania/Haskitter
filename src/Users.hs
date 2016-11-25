module Users where

------------------------------------------------------------------------------
import Control.Applicative
import Snap.Core
import Control.Lens
import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Data.Aeson
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

------------------------------------------------------------------------------
import Application
import Errors

------------------------------------------------------------------------------
data User = User { uid :: Int, email :: String, name :: String, password :: String }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToJSON User where
  toJSON(User uid email name password) =
    object ["id" Data.Aeson..= uid,
            "email" Data.Aeson..= email,
            "name" Data.Aeson..= name]

instance ToRow Int where
  toRow d = [toField d]

data Follow = Follow {follower_id :: Int, followed_id :: Int}

instance FromRow Follow where
  fromRow = Follow <$> field <*> field

instance ToJSON Follow where
  toJSON(Follow follower_id followed_id) =
    object ["follower_id" Data.Aeson..= follower_id,
            "followed_id" Data.Aeson..= followed_id]

-- name is a reserved keyword in postgreSQL, hence it must be escaped in order to do a quer
getUsers :: ExceptT Error AppHandler [User]
getUsers = lift $ with pg $ query_ "SELECT id,email,\"name\",password FROM users"

getUserById :: String -> ExceptT Error AppHandler User
getUserById user_id = getUserByFunction $ (\user -> show (uid user) == user_id)

getUserByEmail :: String -> ExceptT Error AppHandler User
getUserByEmail user_email = getUserByFunction $ (\user -> email user == user_email)

getUserByFunction :: (User -> Bool) -> ExceptT Error AppHandler User
getUserByFunction f = do
  users <- getUsers
  if length (filter f users) /= 0 then lift . return $ head (filter f users) else throwE NoSuchUser

checkPassword :: User -> String -> ExceptT Error AppHandler User
checkPassword user user_password = do
  if password user == user_password then lift . return $ user else throwE InvalidPassword

subscribe :: User -> User -> ExceptT Error AppHandler Follow
subscribe follower followed = do
  lift $ with pg $ execute "INSERT INTO relationships (follower_id,followed_id) VALUES (?,?)" (uid follower,uid followed)
  lift . return $ Follow (uid follower) (uid followed)

signUp :: String -> String -> String -> ExceptT Error AppHandler User
signUp user_email user_name user_password = do
  lift $ with pg $ execute "INSERT INTO users (email,name,password) VALUES (?,?,?)" (user_email,user_name,user_password)
  getUserByEmail user_email

delete :: User -> ExceptT Error AppHandler User
delete user = do
  lift $ with pg $ execute "DELETE FROM users where id = ?" (uid user)
  lift . return $ user
