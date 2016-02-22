module Main(Haskitter,main) where

import Snap
import Snap.Snaplet.Heist
import Control.Lens
import Snap.Snaplet (Handler)
import qualified Data.Text as T
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
--import qualified Post

-- | The Memoise type identifies our application and holds anything our snaplet needs to function.
data Haskitter
  = Haskitter { _heist :: Snaplet (Heist Haskitter)
            }
makeLenses ''Haskitter

instance HasHeist Haskitter where
	heistLens = subSnaplet heist

-- | The indexHandler will be invoked whenever someone accesses the root URL, "/".
indexHandler :: Handler Haskitter Haskitter ()
indexHandler = render "index"

-- | Build a new Memoise snaplet.
hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "Haskell twitter, 'cause YOLO" Nothing $ do
	h <- nestSnaplet "heist" heist $ heistInit "templates"
	addRoutes [("", postsHandler)]
	return $ Haskitter { _heist = h
}

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit -- Initialize a Memoise snaplet
  quickHttpServe site -- Start the Snap server


data Post = Post {
  postId 	::	Integer,
  message	::	T.Text
  } deriving (Eq, Show, Read)

--local database
posts :: [Post]
posts = [Post {postId = 1, message = "hi"},Post{postId = 2,message = "master"} ]

postsHandler :: Handler Haskitter Haskitter ()
postsHandler = renderWithSplices "index" allPostsSplices

allPostsSplices :: Splices (SnapletISplice Haskitter)
allPostsSplices = "posts" ## (renderPosts posts)

renderPosts :: [Post] -> SnapletISplice Haskitter
renderPosts = I.mapSplices $ I.runChildrenWith . postSplices

postSplices :: Monad m => Post -> Splices (I.Splice m)
postSplices post = do
	"postMessage" ## I.textSplice (message post)
