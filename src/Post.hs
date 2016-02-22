module Post(postsHandler) where

import qualified Data.Text as T
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import qualified Main

data Post = Post {
  postId 	::	Integer,
  message	::	T.Text
  } deriving (Eq, Show, Read)

--local database
posts :: [Post]
posts = [Post {postId = 1, message = "hi"},Post{postId = 2,message = "master"} ]

postsHandler :: Handler Hashkitter Hashkitter ()
postsHandler = renderWithSplices "index" allPostsSplices

allPostsSplices :: Splices (SnapletISplice Hashkitter)
allPostsSplices = "posts" ## (renderPosts posts)

renderPosts :: [Post] -> SnapletISplice Hashkitter
renderPosts = I.mapSplices $ I.runChildrenWith . postSplices

postSplices :: Monad m => Post -> Splices (I.Splice m)
postSplices post = do
	"postMessage" ## I.textSplice (message post)
