------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for the
-- site. The 'haskitterInit' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( haskitterInit
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
      , ( "/post"             ,  method POST   $ headersHandler $ runHandler $ genericHandler $ catchHandler $ loginHandler $ postHandler      )
      , ( "/follow"           ,  method POST   $ headersHandler $ runHandler $ genericHandler $ catchHandler $ loginHandler $ followHandler    )
      , ( "/signup"           ,  method POST   $ headersHandler $ runHandler $ genericHandler $ catchHandler $ signUpHandler                   )
      , ( "/user/:id"         ,  method DELETE $ headersHandler $ runHandler $ genericHandler $ catchHandler $ loginHandler $ deleteHandler    )
      ]

------------------------------------------------------------------------------
-- | Build a new Haskitter snaplet.
haskitterInit :: SnapletInit Haskitter Haskitter
haskitterInit = makeSnaplet "hashkitterInit" "A simple twitter written in Haskell" Nothing $ do
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
postsIndexHandler = getPosts

postsWitUserIndexHandler :: ExceptT Error AppHandler [PostWithUser]
postsWitUserIndexHandler = getPostsWithUser

usersIndexHandler :: ExceptT Error AppHandler [User]
usersIndexHandler = getUsers

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
    NullMessage -> "User message is null"
    NullFollowerId -> "Follower id is null"
    InvalidDelete -> "Invalid delete"
    InvalidFollow -> "Invalid follow"
  throwE err

getJSONError :: BS.ByteString -> BS.ByteString
getJSONError error = "{\"error\": \"" `BS.append` error `BS.append` "\"}"

userIdHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
userIdHandler handler = do
  user_id <- lift $ getParam "id"
  user <- maybe (throwE NullId) (\user_id -> getUserById $ (byteStringToString user_id)) user_id
  handler user

userHandler :: User -> ExceptT Error AppHandler User
userHandler user = lift $ return user

-- The parameter mapping decoded from the POST body. Note that Snap only
-- auto-decodes POST request bodies when the request's Content-Type is
-- application/x-www-form-urlencoded. For multipart/form-data use
-- handleFileUploads to decode the POST request and fill this mapping.
-- https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Core.html#v:rqPostParams
loginHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
loginHandler handler = do
  user_email <- nullCheck NullEmail (lift . return) "user_email"
  user_password <- nullCheck NullPassword (lift . return) "user_password"
  user <- getUserByEmail $ (byteStringToString user_email)
  user <- checkPassword user (byteStringToString user_password)
  handler user

feedHandler :: User -> ExceptT Error AppHandler [Post]
feedHandler user = getFollowedPostsByUserId user

postHandler :: User -> ExceptT Error AppHandler Post
postHandler user = do
  user_message <- nullCheck NullMessage (lift . return) "user_message"
  createPost (byteStringToString user_message) user

followHandler :: User -> ExceptT Error AppHandler Follow
followHandler follower = do
  followed_id <- nullCheck NullFollowerId (lift . return) "followed_id"
  followed <- getUserById $ (byteStringToString followed_id)
  if uid follower == uid followed then throwE InvalidFollow else subscribe follower followed

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
    else (do getUserByEmail (byteStringToString user_email); throwE EmailAlreadyTaken) `catchE` (signUpNoSuchUserHandler (byteStringToString user_email) (byteStringToString user_name) (byteStringToString user_password))

signUpNoSuchUserHandler :: String -> String -> String -> Error -> ExceptT Error AppHandler User
signUpNoSuchUserHandler user_email user_name user_password err = -- do
  case err of
    NoSuchUser -> signUp user_email user_name user_password
    _ -> throwE err

deleteHandler :: User -> ExceptT Error AppHandler User
deleteHandler user = do
  user_id <- nullCheck NullId (lift . return) "user_id"
  if (byteStringToString user_id) /= (show $ uid user) then throwE InvalidDelete else delete user
