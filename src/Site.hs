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
        ( "/posts"            ,  method GET    $ headersHandler postsIndexHandler               )
      , ( "/postsWithUser"    ,  method GET    $ headersHandler postsWitUserIndexHandler        )
      , ( "/users"            ,  method GET    $ headersHandler usersIndexHandler               )
      , ( "/user/:id"         ,  method GET    $ headersHandler userHandler                     )
      , ( "/feed/:id"         ,  method GET    $ headersHandler feedHandler                     )
      , ( "/post"             ,  method POST   $ headersHandler $ loginHandler postHandler      )
      , ( "/follow"           ,  method POST   $ headersHandler $ loginHandler followHandler    )
      , ( "/signup"           ,  method POST   $ headersHandler signUpHandler                   )
      , ( "/user/:id"         ,  method DELETE $ headersHandler $ loginHandler deleteHandler    )
      ]

------------------------------------------------------------------------------
-- | Build a new Haskitter snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "A simple twitter written in Haskell" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  addRoutes routes
  return $ Haskitter { _pg = p}

postsIndexHandler :: AppHandler ()
postsIndexHandler = do
  allPosts <- getPosts
  writeLBS . encode $ allPosts

postsWitUserIndexHandler :: AppHandler ()
postsWitUserIndexHandler = do
  posts <- getPostsWithUser
  writeLBS . encode $ posts

usersIndexHandler :: AppHandler ()
usersIndexHandler = do
  users <- getUsers
  writeLBS . encode $ users

headersHandler :: AppHandler () -> AppHandler ()
headersHandler appHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  appHandler

userHandler :: AppHandler ()
userHandler = do
  user_id <- getParam "id"
  maybe (writeBS "User does not exist") userHandler' user_id

userHandler' :: BS.ByteString -> AppHandler ()
userHandler' user_id = do
  maybe_user <- (getUserById $ (byteStringToString user_id))
  checkParam maybe_user "User does not exist" (return ()) (\user -> writeLBS . encode $ user)

-- The parameter mapping decoded from the POST body. Note that Snap only auto-decodes POST request bodies when the request's Content-Type is application/x-www-form-urlencoded. For multipart/form-data use handleFileUploads to decode the POST request and fill this mapping.
-- https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Core.html#v:rqPostParams
loginHandler :: (User -> AppHandler ()) -> AppHandler ()
loginHandler appHandler = do
  user_email    <- getParam "user_email"
  user_password <- getParam "user_password"
  maybe_user    <- checkParam user_email    "No email"    (return Nothing) (\u_email ->
                   checkParam user_password "No password" (return Nothing) (\u_password ->
                   loginHandler' u_email u_password ))
  case maybe_user of
    Nothing -> return ()
    Just user -> appHandler user

invalid_parameter :: BS.ByteString -> AppHandler ()
invalid_parameter message = writeBS message

loginHandler' :: BS.ByteString -> BS.ByteString -> AppHandler (Maybe User)
loginHandler' user_email user_password = do
  maybe_user <- login (byteStringToString user_email) (byteStringToString user_password)
  ifNothingWrite maybe_user "Incorrect login"
  return maybe_user

ifNothingWrite :: Maybe a -> BS.ByteString -> AppHandler ()
ifNothingWrite maybe_var message = do
  case maybe_var of
    Nothing -> writeBS message
    Just var -> return ()

feedHandler :: AppHandler ()
feedHandler = do
  user_id <- getParam "id"
  maybe (invalid_parameter "User does not exist") feedHandler' user_id

feedHandler' :: BS.ByteString -> AppHandler ()
feedHandler' user_id = do
  feed <- getFollowedPostsByUserId (byteStringToString user_id)
  writeLBS . encode $ feed

postHandler :: User -> AppHandler ()
postHandler user = do
  message <- getParam "message"
  checkParam message "No message" (return ()) (\m -> createPost (byteStringToString m) user)

checkParam :: Maybe b -> BS.ByteString -> AppHandler a -> (b -> AppHandler a) -> AppHandler a
checkParam param error_message return_value handler = maybe (do invalid_parameter error_message; return_value) handler param

followHandler :: User -> AppHandler ()
followHandler follower = do
  followed_id     <- getParam "followed_id"
  maybe_followed  <- checkParam  followed_id "No id" (return Nothing) (\f_id ->
                     getUserById (byteStringToString f_id) >>=        (\maybe_user ->
                     do ifNothingWrite maybe_user "User does not exist"; return maybe_user ))
  case maybe_followed of
    Nothing -> return ()
    Just followed -> if uid follower == uid followed then invalid_parameter "You can't follow yourself" else subscribe follower followed

signUpHandler :: AppHandler ()
signUpHandler = do
  user_email                  <- getParam "user_email"
  user_name                   <- getParam "user_name"
  user_password               <- getParam "user_password"
  user_password_confirmation  <- getParam "user_password_confirmation"
  checkParam  user_email                 "No email"                 (return ()) (\u_email ->
   checkParam user_name                  "No name"                  (return ()) (\u_name ->
   checkParam user_password              "No password"              (return ()) (\u_password ->
   checkParam user_password_confirmation "No password confirmation" (return ()) (\u_password_confirmation ->
   signUpHandler' (byteStringToString u_email)
                  (byteStringToString u_name)
                  (byteStringToString u_password)
                  (byteStringToString u_password_confirmation)
                  ))))

signUpHandler' :: String -> String -> String -> String -> AppHandler ()
signUpHandler' user_email user_name user_password user_password_confirmation =
  if user_password /= user_password_confirmation  then
    do
      writeBS "Password confirmation missmatch"
      return ()
  else
    getUserByEmail user_name >>= (\maybe_user ->
                                 case maybe_user of
                                   Just user -> return ()
                                   Nothing   -> signUp user_email user_name user_password )

deleteHandler :: User -> AppHandler ()
deleteHandler user = do
  user_id <- getParam "id"
  checkParam user_id "No id" (return ()) (\u_id ->
   deleteHandler' user (byteStringToString u_id) )

deleteHandler' :: User -> String -> AppHandler ()
deleteHandler' user user_id =
  if user_id /= (show $ uid user) then
    do
      writeBS "Id missmatch"
      return ()
  else
    delete user
