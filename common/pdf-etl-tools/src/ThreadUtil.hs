module ThreadUtil where

-- base:
import Control.Concurrent
import Control.Exception
import System.Posix.Signals

-- | finallyHandlingTerm action handleException finish -
--     like 'Control.Exception.finally' but also add a signal handler (for the TERM signal)
--     to 'action'.  I.e., ensure 'finish' gets executed no matter what.

finallyHandlingTerm :: IO () -> (SomeException -> IO ()) -> IO () -> IO ()
finallyHandlingTerm action handleException finish =
  do
  vStart <- newEmptyMVar
  vDone  <- newEmptyMVar
  let handleTERM tid = do
                       putStrLn "received TERM signal ..."
                       throwTo tid UserInterrupt
                       takeMVar vDone
                       finish
  tid <- forkFinally
           (takeMVar vStart >> action)  -- The main action
           (\x-> do
                 case x of
                   Left e   -> handleException e
                   Right () -> return ()
                 putMVar vDone ()
            )
  _ <- installHandler sigTERM (Catch (handleTERM tid)) Nothing
       -- safe to ignore any previous handler?
  putMVar vStart ()
  takeMVar vDone  -- wait on the 'action'
  finish
