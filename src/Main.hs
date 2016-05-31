module Main(Haskitter,main) where

import Snap
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet (Handler)
import Snap.Snaplet.PostgresqlSimple
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Snap.Extras
import Data.Aeson
import GHC.Generics

-- | The Memoise type identifies our application and holds anything our snaplet needs to function.
data Haskitter = Haskitter
    { _pg :: Snaplet Postgres }

makeLenses ''Haskitter

-- | Build a new Memoise snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "Haskell twitter, 'cause YOLO" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  addRoutes [("/posts", postsIndexHandler),("/users", usersIndexHandler)]
  return $ Haskitter { _pg = p }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit -- Initialize a Memoise snaplet
  quickHttpServe site -- Start the Snap server

-- We shoudl take all this to an external module

data Post = Post { message	::	String } 

data User = User { email :: String, name :: String, password_digest :: String }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance FromRow Post where
  fromRow = Post <$> field

instance ToJSON Post where
  toJSON (Post message) =
    object ["message" Data.Aeson..= message]

instance ToJSON User where
  toJSON(User email name password_digest) =
    object ["name" Data.Aeson..= name]


postsIndexHandler :: Handler Haskitter Haskitter ()
postsIndexHandler = do
  allPosts <- with pg $ query_ "SELECT message FROM posts"
  writeJSON (allPosts :: [Post])

-- name is a reserved keyword in postgreSQL, hence it must be escaped in order to do a quer
usersIndexHandler :: Handler Haskitter Haskitter ()
usersIndexHandler = do
  allUsers <- with pg $ query_ "SELECT email,\"name\",password_digest FROM users"
  writeJSON (allUsers :: [User])