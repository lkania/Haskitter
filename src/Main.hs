module Main(Hashkitter) where

import Snap
import Snap.Snaplet.Heist
import Control.Lens
import Snap.Snaplet (Handler)
import Heist
import qualified Post (postsHandler)

-- | The Memoise type identifies our application and holds anything our snaplet needs to function.
data Hashkitter
  = Hashkitter { _heist :: Snaplet (Heist Hashkitter)
            }
makeLenses ''Hashkitter

instance HasHeist Hashkitter where
	heistLens = subSnaplet heist

-- | The indexHandler will be invoked whenever someone accesses the root URL, "/".
indexHandler :: Handler Hashkitter Hashkitter ()
indexHandler = render "index"

-- | Build a new Memoise snaplet.
hashkitterInit :: SnapletInit Hashkitter Hashkitter
hashkitterInit = makeSnaplet "hashkitterInit" "Haskell twitter, 'cause YOLO" Nothing $ do
	h <- nestSnaplet "heist" heist $ heistInit "templates"
	addRoutes [("", postsHandler)]
	return $ Hashkitter { _heist = h
}

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit -- Initialize a Memoise snaplet
  quickHttpServe site -- Start the Snap server



