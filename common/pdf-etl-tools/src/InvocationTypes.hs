{- | InvocationTypes - Invocation Type and dependents
-}

module InvocationTypes where

-- system:
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           Data.Typeable

-- local:

---- types -------------------------------------------------------------------

type TimeScale    = Int     -- ^ microseconds/byte
type InvokerName  = String  -- ^ (unique) name for an invoker
type Invoker_Src  = Invoker' Arg
type Invoker_Rslt = Invoker' String

data Invocation =
     Invocation { invoker :: Invoker_Rslt
                , file    :: FilePath
                , result  :: Result
                }
     deriving (Eq, Read, Show, Ord, Typeable)

data Invoker' a =
     Invoker { exec         :: [a]
                 -- ^ list of arguments to 'exec'
                 -- constraints:
                 --  - not null
                 --  - first argument must be a valid executable in PATH
             , timeoutScale :: Maybe TimeScale
                 -- ^ allowed microsecs/byte to process file
                 -- Nothing means no timeout.
             , version      :: String
                 -- ^ identifying information re the exec
                 -- e.g., the version number
             , invName      :: InvokerName
                 -- ^ a unique identifier for the invoker
             }
     deriving (Eq, Read, Show, Ord, Typeable)

-- | one argument of the list to pass to 'exec', these 'interpreted' into
-- strings
data Arg    = Str String
                                 -- ^ literal string
            | TmpFN String
                                 -- ^ refers to a process unique temporary
                                 -- filename with the given suffix. (I.e., if you want
                                 -- more than one tmpfile, give a different suffix.)
            | InputFile          -- ^ the path to the input file goes here
            deriving (Eq, Read, Show, Ord, Typeable)

data Result = Timeout    { timeElapsed :: Integer
                         , stdoutRes   :: T.Text
                         , stderrRes   :: T.Text
                         }
            | GoodResult { exitcode    :: Int
                         , timeElapsed :: Integer
                                          -- ^ seconds, of wall-clock-time
                         , stdoutRes   :: T.Text
                         , stderrRes   :: T.Text
                         }
            | RuntimeError { timeElapsed :: Integer
                           , errormsg    :: String
                           }
     deriving (Eq, Read, Show, Ord, Typeable)

---- patterns ----------------------------------------------------------------

pTimeout :: Monad m => Result -> m (Integer, T.Text, T.Text)
pTimeout      (Timeout x1 x2 x3)       = return (x1,x2,x3)
pTimeout      _                        = fail ""

pGoodResult :: Monad m =>
               Result -> m (Int, Integer, T.Text, T.Text)
pGoodResult   (GoodResult x1 x2 x3 x4) = return (x1,x2,x3,x4)
pGoodResult   _                        = fail ""

pRuntimeError :: Monad m => Result -> m (Integer, String)
pRuntimeError (RuntimeError x1 x2)     = return (x1,x2)
pRuntimeError _                        = fail ""

---- misc --------------------------------------------------------------------

printResult :: Result -> IO ()
printResult r =
  case r of
    RuntimeError t s ->
      putLines
        ["time: "     ++ show t
        ,"runtimeError: " ++ s
        ]

    Timeout t out err ->
      do
      putLines
        ["time: "     ++ show t
        ,"timeout (no exitcode)"
        ]
      putOutErr out err

    GoodResult e t out err ->
      do
      putLines
        ["time: "     ++ show t
        ,"exitcode: " ++ show e
        ]
      putOutErr out err

  where
  putLines = mapM_ putStrLn

  putOutErr out err =
    do
    putLines ["stdout:"]
    T.putStrLn out
    putLines ["stderr:"]
    T.putStrLn err
    putChar '\n'
