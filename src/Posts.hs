module Posts where

------------------------------------------------------------------------------
import Control.Applicative
import Snap.Core
import Control.Lens
import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Data.Aeson

------------------------------------------------------------------------------
import Application

------------------------------------------------------------------------------

data Post = Post { message  ::  String } 

instance FromRow Post where
  fromRow = Post <$> field

instance ToJSON Post where
  toJSON (Post message) =
    object ["message" Data.Aeson..= message]

getPosts :: AppHandler [Post]
getPosts = with pg $ query_ "SELECT message FROM posts"