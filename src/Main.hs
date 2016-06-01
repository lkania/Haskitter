module Main(Haskitter,main) where

import Snap
import Control.Lens
import Snap.Snaplet

import Snap.Snaplet.PostgresqlSimple
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Control.Applicative
import Snap.Extras
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import GHC.Generics
import Control.Monad.State.Class
import qualified Data.ByteString as BS

-- | The Memoise type identifies our application and holds anything our snaplet needs to function.
data Haskitter = Haskitter
    { _pg :: Snaplet Postgres }

makeLenses ''Haskitter

-- | Build a new Memoise snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "Haskell twitter, 'cause YOLO" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  addRoutes [("/posts", postsIndexHandler),("/users", usersIndexHandler),("/user/:id",userHandler)]
  return $ Haskitter { _pg = p }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit -- Initialize a Memoise snaplet
  quickHttpServe site -- Start the Snap server

-- We shoudl take all this to an external module

data Post = Post { message	::	String } 

data User = User { uid :: Int, email :: String, name :: String, password_digest :: String }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance FromRow Post where
  fromRow = Post <$> field

instance ToJSON Post where
  toJSON (Post message) =
    object ["message" Data.Aeson..= message]

instance ToJSON User where
  toJSON(User uid email name password_digest) =
    object ["name" Data.Aeson..= name, "id" Data.Aeson..= uid]

instance HasPostgres (Handler b Haskitter) where
	getPostgresState = with pg get

postsIndexHandler :: Handler Haskitter Haskitter ()
postsIndexHandler = do
  allPosts <- query_ "SELECT message FROM posts"
  writeJSON (allPosts :: [Post])

-- name is a reserved keyword in postgreSQL, hence it must be escaped in order to do a quer
usersIndexHandler :: Handler Haskitter Haskitter ()
usersIndexHandler = do
	modifyResponse $ setHeader "Content-Type" "application/json"
	users <- getUsers
	writeLBS . encode $ (users :: [User])

userHandler :: Handler Haskitter Haskitter ()
userHandler = do
	modifyResponse $ setHeader "Content-Type" "application/json"
	user_id <- getParam "id"
	maybe (writeBS "User does not exist") userHandler' user_id

userHandler' :: BS.ByteString -> Handler Haskitter Haskitter ()
userHandler' user_id = do
	user <- (getUser $ (T.unpack $ E.decodeUtf8 user_id))
	writeLBS . encode $ (user :: User) 

-- (Handle b v) is a Monad
getUsers :: Handler Haskitter Haskitter [User]
getUsers = query_ "SELECT id,email,\"name\",password_digest FROM users"

getUser :: String -> Handler Haskitter Haskitter User
getUser user_id = getUsers >>= (\users -> return (head $ (filter (equal_user_id user_id) users)))

equal_user_id :: String -> User -> Bool
equal_user_id user_id user = (show (uid user)) == user_id