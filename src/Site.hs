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
import Errors

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(BS.ByteString, AppHandler ())]
routes = [
        ( "/posts"            ,  method GET    $ headersHandler $ runHandler postsIndexHandler  )
      , ( "/postsWithUser"    ,  method GET    $ headersHandler $ runHandler postsWitUserIndexHandler )
      , ( "/users"            ,  method GET    $ headersHandler $ runHandler usersIndexHandler  )
      , ( "/user/:id"         ,  method GET    $ headersHandler $ runHandler $ genericHandler $ userIdHandler $ userHandler         )
      , ( "/feed/:id"         ,  method GET    $ headersHandler $ runHandler $ genericHandler $ userIdHandler $ feedHandler        )
      , ( "/post"             ,  method POST   $ headersHandler $ loginHandler postHandler      )
      , ( "/follow"           ,  method POST   $ headersHandler $ loginHandler followHandler    )
      , ( "/signup"           ,  method POST   $ headersHandler signUpHandler                   )
      , ( "/user/:id"         ,  method DELETE $ headersHandler $ loginHandler deleteHandler    )
      ]

------------------------------------------------------------------------------
-- |


------------------------------------------------------------------------------
-- | Build a new Haskitter snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "A simple twitter written in Haskell" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  addRoutes routes
  return $ Haskitter { _pg = p}

postsIndexHandler :: ExceptT Error AppHandler ()
postsIndexHandler = do
  allPosts <- getPosts'
  lift $ writeLBS . encode $ allPosts

postsWitUserIndexHandler :: ExceptT Error AppHandler ()
postsWitUserIndexHandler = do
  posts <- getPostsWithUser
  lift $ writeLBS . encode $ posts

usersIndexHandler :: ExceptT Error AppHandler ()
usersIndexHandler = do
  users <- getUsers'
  lift $ writeLBS . encode $ users

headersHandler :: AppHandler () -> AppHandler ()
headersHandler appHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  appHandler

printError :: Error -> ExceptT Error AppHandler a
printError err = do
  lift . writeBS $ case err of
    NullId -> "{\"error\": \"User id is null\"}"
    NoSuchUser -> "{\"error\": \"User does not exist\"}"
  throwE err

runHandler :: ExceptT Error AppHandler () -> AppHandler ()
runHandler handler = do
  runExceptT handler
  return ()

genericHandler :: ToJSON a => ExceptT Error AppHandler a -> ExceptT Error AppHandler ()
genericHandler handler = do
  obj <- handler `catchE` printError
  lift $ writeLBS . encode $ obj

userIdHandler :: (BS.ByteString -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
userIdHandler handler = do
  user_id <- lift $ getParam "id"
  maybe (throwE NullId) handler user_id

userHandler :: BS.ByteString -> ExceptT Error AppHandler User
userHandler user_id = getUserById' $ (byteStringToString user_id)

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

feedHandler :: BS.ByteString -> ExceptT Error AppHandler [Post]
feedHandler user_id = getFollowedPostsByUserId (byteStringToString user_id)

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
