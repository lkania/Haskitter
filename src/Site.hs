------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for the
-- site. The 'haskitterInit' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( hashkitterInit
  ) where

------------------------------------------------------------------------------
import            Control.Applicative
import qualified  Data.ByteString as BS
import            Snap.Core
import            Control.Lens
import            Snap
import            Snap.Snaplet
import            Snap.Snaplet.PostgresqlSimple
import            Snap.Snaplet.Session
import            Snap.Extras
import            Data.Aeson
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession

------------------------------------------------------------------------------
import Application
import Users
import Posts
import Login
import Helpers

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(BS.ByteString, AppHandler ())]
routes = [
        ("/posts", postsIndexHandler)
      , ("/users", usersIndexHandler)
      , ("/user/:id",userHandler)
      , ("/login",method POST loginHandler)
      , ("/logincheck",loginCheckHandler)
      ]

------------------------------------------------------------------------------
-- | Build a new Haskitter snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "Haskell twitter, 'cause YOLO" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  addRoutes routes 
  return $ Haskitter { _pg = p, _sess = s }

postsIndexHandler :: AppHandler ()
postsIndexHandler = do
  allPosts <- getPosts
  writeLBS . encode $ allPosts

usersIndexHandler :: AppHandler ()
usersIndexHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  users <- getUsers
  writeLBS . encode $ users

userHandler :: AppHandler ()
userHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  user_id <- getParam "id"
  maybe (writeBS "User does not exist") userHandler' user_id

userHandler' :: BS.ByteString -> AppHandler ()
userHandler' user_id = do
  user <- (getUserById $ (byteStringToString user_id))
  writeLBS . encode $ (user :: User) 

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