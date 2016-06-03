module Login where

------------------------------------------------------------------------------
import Control.Applicative
import Snap.Core
import Control.Lens
import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session

------------------------------------------------------------------------------
import Application
import Users
import Helpers

------------------------------------------------------------------------------

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

login :: String -> String -> AppHandler Bool
login user_email user_password_digest = do
  user <- getUserByEmail user_email
  if checkPassword user user_password_digest then loginSuccess else return False

loginSuccess :: AppHandler Bool
loginSuccess = do
  logUser
  return True