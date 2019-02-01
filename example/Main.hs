{-# LANGUAGE OverloadedStrings, CPP #-}
module Main where

import System.Environment
import Codec.JVM
import Codec.JVM.Parse
import Codec.JVM.ASM.Code.Instr
import Codec.JVM.ConstPool
import Data.Monoid((<>))

import Data.Text (Text)
import qualified Data.ByteString as BS
{-
#ifdef ETA_VERSION
import GHC.IO (trampolineIO)
#else
trampolineIO = id
#endif

main :: IO ()
main = do
  [file] <- getArgs
  res <- parse file
  print res
-}

main :: IO ()
main = BS.writeFile "Test.class" $ classFileBS classFile

mainClass :: Text
mainClass = "Test"

classFile :: ClassFile
classFile = mkClassFileWithAttrs java7 [Public, Super] mainClass Nothing [] [] [srcFile]
  [
    mkMethodDef mainClass [Public, Static] "main" [jarray jstring] void $
        startLabel loop
     <> markStackMap
     <> emitLineNumber (ln 5)
     <> iconst jint 1
     <> iconst jint 1
     <> iadd
     <> ifeq (goto loop) mempty
     <> vreturn
  ] (const False)
  where srcFile = mkSourceFileAttr "Main.hs"
        loop = mkLabel 1
        ln = mkLineNumber

dumpStackMap :: Code -> IO ()
dumpStackMap (Code consts instr) = do
  putStrLn "Control Flow:"
  print cf
  where cp = mkConstPool consts
        (_, cf, smt) = runInstrBCS instr cp
