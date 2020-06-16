module ThreadUtil where

-- base:
import Control.Concurrent
import Control.Exception
import System.Posix.Signals

-- | finally_WithSignals action handleException final -
--     like 'Control.Exception.finally' but also add a signal handler (for the TERM,.. signals)
--     to 'action'.  I.e., ensure 'finish' gets executed no matter what.

finally_WithSignals :: IO () -> (SomeException -> IO ()) -> IO () -> IO ()
finally_WithSignals action handleException final =
  do
  vStart <- newEmptyMVar
  vDone  <- newEmptyMVar

  -- main thread:
  tid <- forkFinally
           (takeMVar vStart   -- i.e., wait for signal handler installs
            >> action         -- The main action
           )
           (\x-> do
                 case x of
                   Left e   -> handleException e
                   Right () -> return ()
                 putMVar vDone ()
            )

  -- signal handlers:
  let handleSignal s =
        do
        putStrLn $ "received signal " ++ show s
        throwTo tid UserInterrupt
        takeMVar vDone
        final
  _ <- installHandler sigTERM (Catch (handleSignal sigTERM)) Nothing
  _ <- installHandler sigINT  (Catch (handleSignal sigINT )) Nothing
       -- safe to ignore any previous handler?

  putMVar vStart ()
  takeMVar vDone  -- wait on the 'action'
  final
