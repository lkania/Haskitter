module Login where

------------------------------------------------------------------------------
import Control.Applicative
import Snap.Core
import Control.Lens
import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
import Application
import Users
import Helpers

------------------------------------------------------------------------------

-- TODO: Read and understand more this line
-- Make a following of the types

login :: String -> String -> AppHandler (Maybe User)
login user_email user_password = do
  user <- getUserByEmail user_email
  return $ if checkPassword user user_password then Just user else Nothing
