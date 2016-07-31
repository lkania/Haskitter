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
  maybe_user <- getUserByEmail user_email
  return $ case maybe_user of
    Nothing -> Nothing
    Just user -> if checkPassword user user_password then Just user else Nothing
