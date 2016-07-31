------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Snap
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
-- | The Haskitter type identifies our application and holds anything our snaplet needs to function.
data Haskitter = Haskitter
    { _pg   :: Snaplet Postgres }

makeLenses ''Haskitter

------------------------------------------------------------------------------
-- | (Handle b v) is a Monad
type AppHandler = Handler Haskitter Haskitter
