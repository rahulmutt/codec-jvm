{-# LANGUAGE OverloadedStrings #-}
module Codec.JVM.Encoding (encodeModifiedUtf8) where

import Data.ByteString.Lazy as B
import Data.ByteString.Search
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Char
import Data.Monoid
import Data.Bits

{- TODO: This could probably be optimized.
         It was written assuming most strings will contain streams of BMP characters or
         supplementary characters. -}
encodeModifiedUtf8 :: Text -> ByteString
encodeModifiedUtf8 t = goUtf8 t
  where goUtf8 t
          | T.null t  = mempty
          | otherwise = fromStrict (encodeUtf8 utf8) <> goOther other
          where (utf8, other) = T.break (\c -> ord c >= 0x10000 || c == '\NUL') t
        goOther t
          | T.null t  = mempty
          | otherwise = enc other <> goUtf8 utf8
          where (p, enc)
                  | c == '\NUL' = (\c -> c /= '\NUL',
                                   \t -> B.take (fromIntegral (2 * T.length t))
                                       $ B.cycle zeroEncoding)
                  | otherwise    = (\c -> ord c < 0x10000, encodeSurrogates)
                (other, utf8) = T.break p t
                c = T.head t

encodeSurrogates :: Text -> ByteString
encodeSurrogates t = B.pack $ T.foldr (\c t -> calcSurrogates c <> t) [] t
  where calcSurrogates c = [0xED, bits1, bits2, 0xED, bits3, bits4]
          where sub  = ord c - 0x10000
                bits1 = fromIntegral $ 0xA0 .|. (sub `shiftR` 16)
                bits2 = fromIntegral $ 0x80 .|. ((sub `shiftR` 10) .&. 0x3F)
                bits3 = fromIntegral $ 0xB0 .|. ((sub `shiftR` 6) .&. 0x0F)
                bits4 = fromIntegral $ 0x80 .|. (sub .&. 0x3F)

zeroEncoding :: ByteString
zeroEncoding = "\192\128"
