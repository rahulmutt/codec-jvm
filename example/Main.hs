{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.JVM
import Codec.JVM.ASM.Code.Instr
import Codec.JVM.ConstPool
import Data.Monoid((<>))

import Data.Text (Text)
import qualified Data.ByteString as BS

main :: IO ()
main = --BS.writeFile "Test.class" $ classFileBS classFile
  dumpStackMap $
      new (obj "A")
   <> dup (obj "A")
   <> gload jobject 2
   <> gload jobject 3
   <> gload jobject 4
   <> gload jint 5
   <> iconst jint 0
   <> iconst jint 0
   <> invokespecial (mkMethodRef "A" "<init>" (replicate 3 jobject ++ replicate 3 jint) void)

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

dumpStackMap :: Code -> IO ()
dumpStackMap (Code consts instr) = do
  putStrLn "Control Flow:"
  print cf
  where cp = mkConstPool consts
        (_, cf, smt) = runInstrBCS instr cp
