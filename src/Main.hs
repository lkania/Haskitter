module Main(main) where

import Snap.Snaplet
import Snap

import Site

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing haskitterInit
  quickHttpServe site -- Start the Snap server
