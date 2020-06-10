{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base:
import Control.Concurrent
import Control.Exception
import System.Environment
import System.Posix.Process (getProcessID)
import System.Posix.Signals

-- unliftio:
import UnliftIO.Temporary

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

    ["kill"]  -> (do
                  m <- getSignalMask
                  print $ map (\s-> inSignalSet s m) [sigABRT,sigHUP,sigINT,sigKILL,sigQUIT,sigTERM]
                  testSubProcessKill
                  putStrLn "or kill me in the next 5 secs ..."
                  threadDelay (5*1000000) -- 5 more seconds
                  putStrLn "done delaying."
                 )
                 `finally`
                 (do
                  putStrLn "finally..."
                 )

    ["kill2"] -> withSystemTempDirectory "pdf-etl-tool-"
                   (\nm->
                    do
                    putStrLn $ "directory: " ++ nm
                    testSubProcessKill
                    putStrLn "or kill me in the next 5 secs ..."
                    threadDelay (5*1000000) -- 5 more seconds
                    putStrLn "done delaying."
                   )
                 -- this doesn't cleanup on signals

    ["kill3"] -> do
                 vStart <- newEmptyMVar
                 vDone  <- newEmptyMVar
                 let handleTERM tid =
                                  do
                                  putStrLn "got TERM signal"
                                  throwTo tid UserInterrupt
                                  putStrLn "waiting for process to stop"
                                  takeMVar vDone
                                  putStrLn "cleanup code here"
                 tid <- forkFinally
                   (do
                    takeMVar vStart
                    testSubProcessKill
                    putStrLn "or kill me in the next 5 secs ..."
                    threadDelay (5*1000000) -- 5 more seconds
                    putStrLn "done delaying.")
                   (\x->
                      do
                      case x of
                        Left (SomeException e) -> putStrLn $ "Left: " ++ displayException e
                        Right ()               -> putStrLn $ "Right: normal finish"
                      putMVar vDone ()
                   )
                 _ <- installHandler sigTERM (Catch (handleTERM tid)) Nothing
                   -- safe to ignore any previous handler?
                 putMVar vStart ()
                 putStrLn "main: waiting"
                 takeMVar vDone
                 putStrLn "the real exit"

    _         -> putStrLn "Usage: testing [mongo|kill|kill2|kill3]"

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
