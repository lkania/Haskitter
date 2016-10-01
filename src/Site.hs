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
        ( "/posts"            ,  method GET    $ headersHandler $ runHandler $ genericHandler $ postsIndexHandler  )
      , ( "/postsWithUser"    ,  method GET    $ headersHandler $ runHandler $ genericHandler $ postsWitUserIndexHandler )
      , ( "/users"            ,  method GET    $ headersHandler $ runHandler $ genericHandler $ usersIndexHandler  )
      , ( "/user/:id"         ,  method GET    $ headersHandler $ runHandler $ genericHandler $ catchHandler $ userIdHandler $ userHandler         )
      , ( "/feed/:id"         ,  method GET    $ headersHandler $ runHandler $ genericHandler $ catchHandler $ userIdHandler $ feedHandler        )
      , ( "/post"             ,  method POST   $ headersHandler $ loginHandler postHandler      )
      , ( "/follow"           ,  method POST   $ headersHandler $ loginHandler followHandler    )
      , ( "/signup"           ,  method POST   $ headersHandler $ runHandler $ genericHandler $ catchHandler $ signUpHandler                   )
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

headersHandler :: AppHandler () -> AppHandler ()
headersHandler appHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  appHandler

runHandler :: ExceptT Error AppHandler () -> AppHandler ()
runHandler handler = do
  runExceptT handler
  return ()

genericHandler :: ToJSON a => ExceptT Error AppHandler a -> ExceptT Error AppHandler ()
genericHandler handler = do
  obj <- handler
  lift $ writeLBS . encode $ obj

catchHandler :: ExceptT Error AppHandler a -> ExceptT Error AppHandler a
catchHandler handler = handler `catchE` printError

postsIndexHandler :: ExceptT Error AppHandler [Post]
postsIndexHandler = getPosts'

postsWitUserIndexHandler :: ExceptT Error AppHandler [PostWithUser]
postsWitUserIndexHandler = getPostsWithUser

usersIndexHandler :: ExceptT Error AppHandler [User]
usersIndexHandler = getUsers'

printError :: Error -> ExceptT Error AppHandler a
printError err = do
  lift . writeBS . getJSONError $ case err of
    NullId ->  "User id is null"
    NoSuchUser -> "User does not exist"
    EmailAlreadyTaken -> "Email already taken"
    NullEmail -> "User email is null"
    NullName -> "User name is null"
    NullPassword -> "User password is null"
    NullPasswordConfirmation -> "User password confirmation is null"
    PasswordConfirmationMissmatch -> "There was a missmatch between user password and user password confirmation"
  throwE err

getJSONError :: BS.ByteString -> BS.ByteString
getJSONError error = "{\"error\": \"" `BS.append` error `BS.append` "\"}"

userIdHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
userIdHandler handler = do
  user_id <- lift $ getParam "id"
  user <- maybe (throwE NullId) (\user_id -> getUserById' $ (byteStringToString user_id)) user_id
  handler user

userHandler :: User -> ExceptT Error AppHandler User
userHandler user = lift $ return user

-- The parameter mapping decoded from the POST body. Note that Snap only
-- auto-decodes POST request bodies when the request's Content-Type is
-- application/x-www-form-urlencoded. For multipart/form-data use
-- handleFileUploads to decode the POST request and fill this mapping.
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

feedHandler :: User -> ExceptT Error AppHandler [Post]
feedHandler user = getFollowedPostsByUserId user

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

nullCheck :: Error -> (BS.ByteString -> ExceptT Error AppHandler BS.ByteString) -> BS.ByteString -> ExceptT Error AppHandler BS.ByteString
nullCheck error f object_id = do
  maybe_object <- lift $ getParam object_id
  maybe (throwE error) f maybe_object

signUpHandler :: ExceptT Error AppHandler User
signUpHandler = do
  user_email <- nullCheck NullEmail (lift . return) "user_email"
  user_name <- nullCheck NullName (lift . return) "user_name"
  user_password <- nullCheck NullPassword (lift . return) "user_password"
  user_password_confirmation <- nullCheck NullPasswordConfirmation (lift . return) "user_password_confirmation"
  if user_password /= user_password_confirmation
    then throwE PasswordConfirmationMissmatch
    else (do getUserByEmail' (byteStringToString user_email); throwE EmailAlreadyTaken) `catchE` (signUpNoSuchUserHandler (byteStringToString user_email) (byteStringToString user_name) (byteStringToString user_password))

signUpNoSuchUserHandler :: String -> String -> String -> Error -> ExceptT Error AppHandler User
signUpNoSuchUserHandler user_email user_name user_password err = do
  case err of
    NoSuchUser -> signUp user_email user_name user_password
    EmailAlreadyTaken -> throwE EmailAlreadyTaken
  throwE err

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
