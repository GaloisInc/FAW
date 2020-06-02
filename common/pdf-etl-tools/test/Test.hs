{-# OPTIONS_GHC -Wno-missing-signatures -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- without both of these, the Hs syntax for docs/queries would not compile!

module Test where

import           Control.Monad
import           Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO

-- package: bson-mapping:
import Data.Bson.Mapping

-- package: mongoDB:
import           Database.MongoDB

-- local modules:
import           CommandLineProcessing
import           Database
import           Invocation
import           Types

-- NOTE: specializing things to this:
dfltOpts = Options
             { opt_database  = T.pack "littlegovdocs"
             , opt_collection= T.pack "rawinvocations"
             , opt_server    = serverDefault
             , opt_verbose   = verbosely
             }
dataDir = "../test/data"

mySelectAndProject' = mySelectAndProject dfltOpts


---- queries: useful, examples -----------------------------------------------

queryFilenames =
  mySelectAndProject' ["invoker.exec" =: "qpdf"]
                      ["file" =: 1]

queryQpdfExitCode0 =
   mySelectAndProject' ["invoker.exec" =: "qpdf", "result.exitcode" =: 0] []
   -- Q. need to add ["result._cons" =: "GoodResult"] ??

queryTime =
    mySelectAndProject'
      ["result._cons" =: "GoodResult"]
      ["invoker.exec" =: 1, "file" =: 1, "result.timeElapsed" =: 1]

queryPython  = mySelectAndProject' ["invoker.exec" =: "python"] []

queryTimeouts = mySelectAndProject' ["result._cons" =: "Timeout"] []


---- testing Misc ------------------------------------------------------------

testq1 = mySelectAndProject' ["result._cons" =: "RuntimeError"] []
testq2 = mySelectAndProject' ["result._cons" =: "GoodResult"] []


---- testing database interactions -------------------------------------------

-- invoke & insert one:
test3' o = runInDB o $
           do
           x <- liftIO (invokeVerbosely qpdf   (dataDir </> "000009.pdf"))
           y <- liftIO (invokeVerbosely pdfId  (dataDir </> "000029.pdf"))
           insertMany (opt_collection o) (map toBson [x,y])

test3 = test3' dfltOpts

---- testing invokers --------------------------------------------------------
-- e.g., on all files

applyInvokerToAllTests i =
  do
  fs <- listDirectory dataDir
  forM fs $ \f->
    invokeVerbosely i (dataDir </> f)

---- invokers ----

-- FIXME: nab the version info automatically!

-- TimeScale is microseconds/byte allowed for execution.
--   largest in govdocs:
--    -r--r--r-- 1 nobody 72168407 Jul 29  2005 475419.pdf
--   rounding it to 10^8
-- let's give it 30 minutes for long invocations:
-- longTimeoutScale = Just (round(30*60*10^6 / 10^8)) :: Maybe TimeScale

-- let's give it  5 minutes for quick invocations:
-- shortTimeoutScale = Just(round(5*60*10^6 / 10^8)) :: Maybe TimeScale


invokerList :: [Invoker_Src]
invokerList =
  [ Invoker
      [A_Str "pdfinfo", A_Input]
      shortTimeoutScale
      "poppler-0.62.0"
      "pdfinfo"
  , Invoker
      [A_Str "pdfinfo", A_Str "-meta", A_Input]
      shortTimeoutScale
      "poppler-0.62.0"
      "pdfinfo-meta"
  , Invoker
      [A_Str "pdfinfo", A_Str "-struct", A_Input]
      longTimeoutScale
      "poppler-0.62.0"
      "pdfinfo-struct"
  , Invoker
      [A_Str "pdftocairo", A_Str "-pdf", A_Input, A_TmpFN "pdftocairo.pdf"]
      longTimeoutScale
      "poppler-0.62.0"
      "pdftocairo-pdf"
  , Invoker
      [A_Str "qpdf", A_Str "--check", A_Input]
      longTimeoutScale
      "8.0.2"
      "qpdf-check"
  , Invoker
      [A_Str "python", A_Str "../lib/pdfid_v0_2_5/pdfid.py", A_Str "-e", A_Input]
      longTimeoutScale
         -- shortTimeoutScale was timing out!
         --   because this scans whole file?
      "pdfid_v0_2_5"
       -- FIXME: remove dependence on CWD
      "pdfid"
  , Invoker
      [A_Str "caradoc", A_Str "stats", A_Input]
      longTimeoutScale
      "caradoc-0.3"
      "caradoc-stats"
  , Invoker
      [A_Str "caradoc", A_Str "stats", A_Str "--strict", A_Input]
      longTimeoutScale
      "caradoc-0.3"
      "caradoc-strict"
  , Invoker
      -- this segfaults, for testing!
      [A_Str "./c-src/die", A_Input]
      shortTimeoutScale
      ""
      "die-test"
  ]
  -- FIXME: add polytracker
  -- FIXME: add galois parser

{-
test_0         =
pdfinfo_0      =
pdfinfo_meta   =
pdfinfo_struct =
poppler_c      =
qpdf           =
pdfId          =
caradocStats   =
caradocStatsStrict
               =
-}


---- testing files -----------------------------------------------------------

-- some problematic pdfs:
ps =
 [ "007526.pdf"
 , "099692.pdf"
 , "736151.pdf"
 , "974733.pdf"
 ]

t01 = invokeVerbosely pdfinfo_struct (dataDir </> "099692.pdf")

t02 =
  do
  s <- readFile "T-7526"
  mapM_ print (lines s)

t02b =
  do
  h <- openBinaryFile "T-7526" ReadMode
  s <- hGetContents h
  mapM_ print (lines s)

test_ps =
  forM ps
    (\f-> do
          putStrLn f
          rs <- applyAllInvokersToFile quietly f
          prResultsBriefly rs
          putStrLn ""
          return rs
    )

prResultsBriefly = mapM_ (print . take 60 . show . result)

applyAllInvokersTo_p1 v = applyAllInvokersToFile v "007526.pdf"


applyAllInvokersTo9 v = applyAllInvokersToFile v "000009.pdf"
applyAllInvokersToFile v f =
  applyInvokersTo dfltOpts{opt_verbose=v} (dataDir </> f) allInvokersIndices


---- misc --------------------------------------------------------------------

test0 = do
        r1 <- invokeVerbosely test_0 (dataDir </> "000009.pdf")  -- gives segfault
        print (toBson r1)

test1 = do
        r1 <- invokeVerbosely qpdf  (dataDir </> "000009.pdf")
        print (toBson r1)

test1b = do
         r1 <- invokeVerbosely
                  (qpdf{timeoutScale=Just 1})
                  (dataDir </> "000012.pdf")
         print (toBson r1)
test1c = do
         r1 <- invokeVerbosely
                  poppler_c
                  (dataDir </> "000012.pdf")
         print (toBson r1)

---- cmdLineParser -----------------------------------------------------------

testCLP =
 do
 args <- getArgs
 print args
 o <- cmdLineParser
 putStrLn "hello!"
 print o


---- old-ish tests -----------------------------------------------------------
{-
main3 = do
        test4io verbosely

test4io v = mapM (invokeOnEachInDataDir v)   [qpdf,pdfId]
test4ii v = invokeOnEachInDataDirAndInsert v [qpdf,pdfId]


invokeOnEachInDataDir :: Options -> O_AddRaw -> Invoker -> IO [Invocation]
invokeOnEachInDataDir o co i =
  do
  fs <- listDirectory dataDir
  forM fs $ \f->
    invoke' (opt_verbose o) i (dataDir </> f)

  where
  dataDir = ar_dirname co

invokeOnEachInDataDirAndInsert ::
  Options -> O_AddRaw -> [Invoker] -> IO [Value]
invokeOnEachInDataDirAndInsert o co is =
  runInDB (opt_database o) $
    do
    rss <- liftIO $ mapM (invokeOnEachInDataDir o co) is
    insertMany (opt_collection o) (map toBson (concat rss))
-}
