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
import            Snap.Extras
import            Data.Aeson

------------------------------------------------------------------------------
import Application
import Users
import Posts
import Login
import Helpers
import Feed

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(BS.ByteString, AppHandler ())]
routes = [
        ( "/posts"     ,  method GET   postsIndexHandler         )
      , ( "/users"     ,  method GET    usersIndexHandler         )
      , ( "/user/:id"  ,  method GET    userHandler               )
      , ( "/feed/:id"  ,  method GET    feedHandler               )
      , ( "/post"      ,  method POST $ loginHandler postHandler  )
      --, ( "/subscribe" ,  method POST $ subscribeHandler          )
      ]

------------------------------------------------------------------------------
-- | Build a new Haskitter snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "Haskell twitter, 'cause YOLO" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  addRoutes routes 
  return $ Haskitter { _pg = p}

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
  writeLBS . encode $ user

-- The parameter mapping decoded from the POST body. Note that Snap only auto-decodes POST request bodies when the request's Content-Type is application/x-www-form-urlencoded. For multipart/form-data use handleFileUploads to decode the POST request and fill this mapping.
-- https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Core.html#v:rqPostParams
loginHandler :: (User -> AppHandler ()) -> AppHandler ()
loginHandler appHandler = do 
  user_email <- getParam "user_email"
  user_password <- getParam "user_password"
  maybe_user <- checkParam user_email (\u_email -> checkParam user_password (\u_password -> loginHandler' u_email u_password) "No password" (return Nothing)) "No email" (return Nothing)
  case maybe_user of
    Nothing -> return ()
    Just user -> appHandler user

invalid_parameter :: BS.ByteString -> AppHandler ()
invalid_parameter message = do
  writeBS message

loginHandler' :: BS.ByteString -> BS.ByteString -> AppHandler (Maybe User)
loginHandler' user_email user_password = do
  maybe_user <- login (byteStringToString user_email) (byteStringToString user_password)
  case maybe_user of
    Nothing -> writeBS "Incorrect login"
    Just user -> return ()
  return maybe_user  

feedHandler :: AppHandler ()
feedHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  user_id <- getParam "id"
  maybe (writeBS "User does not exist") feedHandler' user_id

feedHandler' :: BS.ByteString -> AppHandler ()
feedHandler' user_id = do
  feed <- getFollowedPostsByUserId (byteStringToString user_id)
  writeLBS . encode $ feed

postHandler :: User -> AppHandler ()
postHandler user = do
  message <- getParam "message"
  checkParam message (\m -> post (byteStringToString m) user) "No message" (return ())

checkParam :: Maybe BS.ByteString -> (BS.ByteString -> AppHandler a) -> BS.ByteString -> AppHandler a -> AppHandler a
checkParam param handler error_message return_value = maybe (do invalid_parameter error_message; return_value) handler param