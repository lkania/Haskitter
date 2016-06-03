module Helpers where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified  Data.Text.Encoding as E

------------------------------------------------------------------------------

byteStringToString :: BS.ByteString -> String
byteStringToString = T.unpack . E.decodeUtf8

textToString :: T.Text -> String
textToString = T.unpack
