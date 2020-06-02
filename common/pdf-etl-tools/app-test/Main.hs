{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base:
import System.Environment
import System.Posix.Process (getProcessID)

-- local:
import qualified MongoExample
-- import CommandLineProcessing
import ProcessUtil

main :: IO ()
main =
  do
  args <- getArgs
  case args of
    ["mongo"] -> MongoExample.main2
    ["kill"]  -> testSubProcessKill
    _         -> putStrLn "Usage: testing [mongo|kill]"

testSubProcessKill :: IO ()
testSubProcessKill =
  do
  pid <- getProcessID
  putStrLn $ "Kill me thus (within 10 secs):\n kill " ++ show pid
  r <- readProcessWithExitCodeOrTimeout  -- sleeping 10sec
         "sleep"
         ["10"]
         (Just (12*1000000))  -- giving 2 sec "head start"
         ""
  print r
