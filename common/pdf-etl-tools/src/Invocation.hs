{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Invocation where

import Control.Exception
import Control.Monad
import Data.Typeable
import System.Exit
import System.Posix.Files

-- package: time
import Data.Time

-- package: bson
import Data.Bson

-- package: bson-mapping
import Data.Bson.Mapping

-- local:
import InvocationTypes
import ProcessUtil
import Types
import Util
import TempFiles

---- instances, etc ----------------------------------------------------------

-- NOTE: deriveBson requires that the datatype must name all arguments of its
-- constructors:

$(deriveBson ''Result)

-- writing this by hand because deriveBson doesn't work for parameterized types:
instance Val a => Val(Invoker' a) where
  val (Invoker as sc v name) =
    Doc ["exec"         := val as
        ,"timeoutScale" := val sc
        ,"version"      := val v
        ,"invName"      := val name
        ]

  cast' (Doc["exec"         := es
            ,"timeoutScale" := sc
            ,"version"      := v
            ,"invName"      := n
            ])             = do
                             es' <- cast' es
                             sc' <- cast' sc
                             v'  <- cast' v
                             n'  <- cast' n
                             return (Invoker es' sc' v' n')
  cast' _                  = Nothing

-- FIXME[F2]: here and below: is it safe to assume the database doesn't reorder
-- the fields?

---- Document <-> Invocation -------------------------------------------------

-- We don't put the full Invocation type into our database (via Document), it
-- exposes implementation details to users of the database.

-- | Invoker_Export - a type for writing a custom Val instance.
newtype Invoker_Export a = Invoker_Export (Invoker' a)
                           deriving (Eq, Read, Show, Ord, Typeable)

instance Val a => Val(Invoker_Export a) where
  val (Invoker_Export i) = excludeValue ["exec"] $ val i
  cast' d = Invoker_Export <$> cast' (addExec d)
            where
            addExec (Doc fs) = Doc (("exec" :=  Array []) : fs)
            addExec _        = error "cast'[Invoker_Export]"


docToInvocation :: Document -> Invocation
docToInvocation d =
  case docToInvocation_Safe d of
    Just i  -> i
    Nothing -> error "fail: docToInvocation: wrong version of database?"

docToInvocation_Safe :: Document -> Maybe Invocation
docToInvocation_Safe d =
  case d of
    ["invoker" := i, "file" := f, "result"  := r] ->
        do
        Invoker_Export i' <- cast' i
        f' <- cast' f
        r' <- cast' r
        return (Invocation i' f' r')
    _ ->
        Nothing

invocationToDoc :: Invocation -> Document
invocationToDoc (Invocation i f r) =
  [ "invoker" := val (Invoker_Export i)
  , "file"    := val f
  , "result"  := val r
  ]

excludeValue :: [Label] -> Value -> Value
excludeValue ls (Doc d) = Doc (exclude ls d)
excludeValue _  _       = error "excludeValue"


---- invoking ----------------------------------------------------------------

invokeVerbosely :: TimeoutOverride -> Invoker_Src -> FilePath -> IO Invocation
invokeVerbosely = invoke' verbosely

invoke :: TimeoutOverride -> Invoker_Src -> FilePath -> IO Invocation
invoke = invoke' quietly


computeTimeoutInMicrosecs :: TimeoutOverride -> Maybe TimeScale -> FilePath -> IO (Maybe Int)
computeTimeoutInMicrosecs override scale inputFile =
  do
  case override of
    TO_NoOverride ->
        do
        status <- getFileStatus inputFile
              -- NOTE: this is unnecessary when (isNothing scale)
        return $ (\x-> max (x * fromIntegral (fileSize status))
                         6000000 -- never less than 6 secs
                 ) <$> scale
    TO_NoTimeout ->
        return Nothing
    TO_Timeout i ->
        return $ Just (1000000*i)  -- sec to msec

invoke' :: Verbose -> TimeoutOverride -> Invoker_Src -> FilePath -> IO Invocation
invoke' verbose override i@(Invoker args scale _ver _iname) inputFile =
  do
  y <- fileExist inputFile
  unless y $
    error $ unwords ["file",inputFile,"does not exist"]
  exe:args' <- mapM (interpretArg inputFile) args
    -- NOTE: error if null list!

  when verbose $
    putStrLn $ "invoking " ++ unwords (exe : args')

  mTimeoutMicrosecs <- computeTimeoutInMicrosecs override scale inputFile
  t1 <- getCurrentTime
  r <- catchNonAsyncException
         (fmap Right
           (readProcessWithExitCodeOrTimeout exe args'
              mTimeoutMicrosecs
              "" -- no stdinput
           ))
         (\e-> return (Left (displayException e)))

  t2 <- getCurrentTime
  let
    codeToInt (ExitSuccess)   = 0
    codeToInt (ExitFailure n) = n

    elapsed = ceiling (diffUTCTime t2 t1)

    r' = case r of
           Left msg                 -> RuntimeError elapsed msg
           Right (me,out,err) ->
               case me of
                 Nothing -> Timeout                  elapsed out err
                 Just e  -> GoodResult (codeToInt e) elapsed out err
  when verbose $
    printResult r'

  mapM_ removeTempFileArg args

  return $
     Invocation { invoker= i{exec=exe:args'}
                , file   = inputFile
                , result = r'
                }

interpretArg :: FilePath -> Arg -> IO String
interpretArg ifile a =
  case a of
    Str s          -> return s
    InputFile      -> return ifile
    TmpFN template -> getTempFileName template


removeTempFileArg :: Arg -> IO ()
removeTempFileArg arg =
  case arg of
    TmpFN template ->
        do
        fn <- getTempFileName template
        tryRemoveFile fn
        return ()
    _ ->
        return ()

---- utilities ---------------------------------------------------------------

-- catchNonAsyncException - like catch, but Async Exceptions are *not* caught
catchNonAsyncException :: IO a -> (SomeException -> IO a) -> IO a
catchNonAsyncException action h =
  catchJust
   (\e-> case fromException e of
           Just (_::AsyncException) -> Nothing
                         -- these (ThreadKilled,UserInterrupt,...) are not caught
           Nothing -> Just e
   )
   action
   h
