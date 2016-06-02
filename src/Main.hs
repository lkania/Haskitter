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
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession


-- | The Memoise type identifies our application and holds anything our snaplet needs to function.
data Haskitter = Haskitter
    { _pg   :: Snaplet Postgres
    , _sess :: Snaplet SessionManager
    }

makeLenses ''Haskitter

-- | Build a new Memoise snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "Haskell twitter, 'cause YOLO" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  addRoutes [("/posts", postsIndexHandler),("/users", usersIndexHandler),("/user/:id",userHandler),("/login",method POST loginHandler),("/logincheck",loginCheckHandler)]
  return $ Haskitter { _pg = p, _sess = s }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit -- Initialize a Memoise snaplet
  quickHttpServe site -- Start the Snap server

-- We shoudl take all this to an external module

data Post = Post { message  ::  String } 

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

postsIndexHandler :: AppHandler ()
postsIndexHandler = do
  allPosts <- query_ "SELECT message FROM posts"
  writeJSON (allPosts :: [Post])

-- (Handle b v) is a Monad
type AppHandler = Handler Haskitter Haskitter

-- name is a reserved keyword in postgreSQL, hence it must be escaped in order to do a quer
usersIndexHandler :: AppHandler ()
usersIndexHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  users <- getUsers
  writeLBS . encode $ (users :: [User])

userHandler :: AppHandler ()
userHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  user_id <- getParam "id"
  maybe (writeBS "User does not exist") userHandler' user_id

userHandler' :: BS.ByteString -> AppHandler ()
userHandler' user_id = do
  user <- (getUserById $ (byteStringToString user_id))
  writeLBS . encode $ (user :: User) 

byteStringToString :: BS.ByteString -> String
byteStringToString = T.unpack . E.decodeUtf8

textToString :: T.Text -> String
textToString = T.unpack

getUsers :: AppHandler [User]
getUsers = query_ "SELECT id,email,\"name\",password_digest FROM users"

getUserById :: String -> AppHandler User
getUserById user_id = getUserByFunction $ (\user -> show (uid user) == user_id)

getUserByEmail :: String -> AppHandler User
getUserByEmail user_email = getUserByFunction $ (\user -> email user == user_email)

getUserByFunction :: (User -> Bool) -> AppHandler User
getUserByFunction f = getUsers >>= (\users -> return (head $ (filter f users)))


-- Session.hs https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Session.hs

userIsLoged :: AppHandler Bool
userIsLoged = (with sess $ getFromSession "isLoged") >>= (\isLoged -> return $ userIsLoged' (isLoged >>= (\value -> return $ textToString value)))

userIsLoged' :: Maybe String -> Bool
userIsLoged' isLoged = case isLoged of
                              Nothing -> False
                              Just value -> value == "True" 

-- TODO: Read and understand more this line
-- Make a following of the types
logUser :: AppHandler ()
logUser = withSession sess (with sess $ setInSession "isLoged" "True")

checkPassword :: User -> String -> Bool
checkPassword user user_password_digest = password_digest user == user_password_digest

login :: String -> String -> AppHandler Bool
login user_email user_password_digest = do
  user <- getUserByEmail user_email
  if checkPassword user user_password_digest then loginSuccess else return False

loginSuccess :: AppHandler Bool
loginSuccess = do
  logUser
  return True
-- The parameter mapping decoded from the POST body. Note that Snap only auto-decodes POST request bodies when the request's Content-Type is application/x-www-form-urlencoded. For multipart/form-data use handleFileUploads to decode the POST request and fill this mapping.
-- https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Core.html#v:rqPostParams
loginHandler :: AppHandler ()
loginHandler = do 
  user_email <- getParam "user_email"
  user_password_digest <- getParam "user_password_digest"
  maybe (writeBS "No email") (\u_email -> maybe (writeBS "No password") (loginHandler' u_email) user_password_digest) user_email

loginHandler' :: BS.ByteString -> BS.ByteString -> AppHandler ()
loginHandler' user_email user_password_digest = do
  result <- login (byteStringToString user_email) (byteStringToString user_password_digest)
  if result then writeBS "Loged!" else writeBS "Login fail"  

loginCheckHandler :: AppHandler ()
loginCheckHandler = userIsLoged >>= (\value -> if value then writeBS "Correctly Loged" else writeBS "Not Loged")