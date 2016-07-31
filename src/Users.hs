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

data User = User { uid :: Int, email :: String, name :: String, password_digest :: String }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToJSON User where
  toJSON(User uid email name password_digest) =
    object ["name" Data.Aeson..= name, "id" Data.Aeson..= uid]

-- name is a reserved keyword in postgreSQL, hence it must be escaped in order to do a quer
getUsers :: AppHandler [User]
getUsers = with pg $ query_ "SELECT id,email,\"name\",password_digest FROM users"

getUserById :: String -> AppHandler User
getUserById user_id = getUserByFunction $ (\user -> show (uid user) == user_id)

getUserByEmail :: String -> AppHandler User
getUserByEmail user_email = getUserByFunction $ (\user -> email user == user_email)

getUserByFunction :: (User -> Bool) -> AppHandler User
getUserByFunction f = getUsers >>= (\users -> return (head $ (filter f users)))

checkPassword :: User -> String -> Bool
checkPassword user user_password_digest = password_digest user == user_password_digest