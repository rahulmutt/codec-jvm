{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Encoding (encodeModifiedUtf8) where

import Data.ByteString.Lazy
import Data.ByteString.Search
import Data.Text (Text)
import Data.Text.Encoding

-- WARNING: This does not handle Unicode codepoints beyond 0xFFFF yet.
encodeModifiedUtf8 :: Text -> ByteString
encodeModifiedUtf8 t = replace "\0" ("\192\128" :: ByteString) first
  where first = encodeUtf8 t
