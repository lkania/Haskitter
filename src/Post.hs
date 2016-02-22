module Post(postsHandler) where

import qualified Data.Text as T
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import {-# SOURCE #-} Main

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
