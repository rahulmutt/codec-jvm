{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.JVM
import Data.Monoid((<>))

import Data.Text (Text)
import qualified Data.ByteString as BS

main :: IO ()
main = BS.writeFile "Test.class" $ classFileBS classFile

mainClass :: Text
mainClass = "Test"

classFile :: ClassFile
classFile = mkClassFile java7 [Public, Super] mainClass Nothing [] []
  [
    mkMethodDef mainClass [Public, Static] "main" [jarray jstring] void $
        iconst jint (fromIntegral 1)
     <> ifeq mempty vreturn
     <> vreturn
  ]
