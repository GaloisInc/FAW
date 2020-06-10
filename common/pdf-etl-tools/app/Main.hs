module Main where

-- system packages:
import           Control.Monad
import           Data.Maybe
import qualified Data.Set  as Set
import qualified Data.Text as T

-- package: mongoDB
import qualified Database.MongoDB as DB

-- package: bson:
import           Data.Bson

-- local modules:
import           CommandLineProcessing
import           Database
import           DatabaseIdioms
import           Invocation
import           InvocationTypes
import           Types
import           Util
import           TempFiles

---- the main function -------------------------------------------------------

main :: IO ()
main =
  do
  (opts,cmd) <- cmdLineParser
  globals <- optionsToGlobals opts
  case cmd of
    C_AddRaw copts      -> run_addRaw globals copts
    C_Decisions copts   -> run_decisions globals copts
    C_ListInvokers      -> run_listInvokers globals
    C_Clear             -> run_clear globals
    C_Retry             -> run_retry globals

---- command : retry --------------------------------------------------------

-- | run_retry - this queries the database and redoes every document that
--   is a runtime error.
--
--   FIXME[F2]: must now [as we now have uniqueness keys on database]
--  - either delete before or use 'replace'
--  - see the NIY upsert below:

run_retry :: Globals -> IO ()
run_retry opts =
  do
  createTempDir
  xs <- get_RuntimeErrors opts
  rs <- forM xs $ \(iname,f)->
          case findInvoker opts iname of
            Just i   -> applyInvoker opts TO_NoOverride f i
            Nothing  -> exitWithMessage $
                         "incompatible mongo database: unknown invokerName '"
                           ++iname++"'"

  putStrLn $ show (length xs) ++ " runtime errors"
  putStrLn $ show (length (filter (isJust . pGoodResult . result) rs))
             ++ " good results after re-running"

  {-
   docs <- mySelectAndProject opts ["result._cons" =: "RuntimeError"] []
  -}

  ids <- runInDB opts $
    DB.insertMany (g_collection opts) (map invocationToDoc rs)
  putStrLn $ "new documents added to collection:"
  mapM_ print ids

---- command : addRaw --------------------------------------------------------

run_addRaw :: Globals -> O_AddRaw -> IO ()
run_addRaw opts co =
  do
  createTempDir
  fs <- stdinIfNull $ ar_files co
  is <- case ar_invokers co of
          []      -> error "run_addRaw: internal error"
          ["ALL"] -> return $ g_invokerList opts
          _       -> forM (ar_invokers co) $ \nm->
                       case findInvoker opts nm of
                         Just i  -> return i
                         Nothing -> exitWithMessage $
                                      "unkown invoker: '" ++ nm ++ "'"

  forM_ is $ \i->   -- for each invoker
    do
    putStrLn $ "processing invoker '"++invName i++"':"
    case ar_addStyle co of
      AS_Insert ->
          applyAndInsertAll i fs
      AS_InsertIfAbsent ->
          -- the two cases are for optimizing different use cases:
          case fs of
            []  -> putStrLn " no files specified!"
            [f] ->
                -- single file to be processed, optimizing for this case: we don't
                -- want to query *all* files in the database.
                do
                ds <- mySelectAndProject opts
                                         [ T.pack "invoker.invName" =: invName i
                                         , T.pack "file"            =: f
                                         ]
                                         [T.pack "_id" =: (1 :: Int)] -- not looking inside
                case length ds of
                  0 -> applyAndInsertAll i [f]
                  1 -> putStrLn $ " file '" ++ f ++ "' invocation exists in database, ignoring"
                  _ -> exitWithMessage "database doesn't match expectations"

            _   ->
                -- possibly *many* files to process, let's do one query on the database to get
                -- a (potentially large!) list of all the files in the database.
                do
                existingFiles <-
                  selectInvokersAndProjectField opts (invName i) (T.pack "file")
                  -- assuming sorted b/c
                  -- assuming we have key on file
                  -- ^ nevermind: doesn't seem to remain the case!
                let existingFiles' = Set.fromList existingFiles
                    fs'            = Set.fromList fs
                let (ignoreFiles,addFiles) = Set.partition
                                               (`Set.member` existingFiles')
                                               fs'
                putStrLn $ " "++show(length fs')         ++ " files specified"
                putStrLn $ " "++show(length ignoreFiles) ++ " files will be ignored"
                putStrLn $ " "++show (length addFiles)   ++ " files will be added"

                when (g_verbose opts) $
                  forM_ (Set.toList ignoreFiles) $ \f->
                    putStrLn $ " already in collection: " ++ ppFI f i
                applyAndInsertAll i (Set.toList addFiles)

  where
  applyAndInsertAll i fs =
    forM_ fs $ \f->
      do
      putStrLn $ " adding to collection: " ++ ppFI f i
      unless (ar_dry co) $
        do
        r <- applyInvoker opts (ar_timeout co) f i
        unless (ar_nodb co) $
          void $ runInDB opts (DB.insert (g_collection opts) (invocationToDoc r))

  ppFI f i = concat ["file '",f,"', invoker '",invName i,"'"]

---- command : decision ------------------------------------------------------

run_decisions :: Globals -> O_Decisions -> IO ()
run_decisions opts co =
  do
  fs <- stdinIfNull $ d_files co
  forM_ fs $ \f->
    decideFile opts f
  -- NOTE: this gives us a separate connection/transaction for each file.


---- command : clear ---------------------------------------------------------

run_clear :: Globals -> IO ()
run_clear opts =
  do
  when (g_verbose opts)
    $ putStrLn "clearing collection"
  runInDB opts
          (DB.delete (DB.select [] (g_collection opts)))

---- command : listInvokers --------------------------------------------------

run_listInvokers :: Globals -> IO ()
run_listInvokers opts =
  mapM_
    (if g_verbose opts then
       print
     else
       (putStrLn . invName))
    (g_invokerList opts)

---- invoker 'utilities' -----------------------------------------------------

applyInvoker :: Globals -> TimeoutOverride -> FilePath -> Invoker_Src -> IO Invocation
applyInvoker opts timeout fn i =
  do
  when (g_verbose opts) $
    print i
  r <- invoke' (g_verbose opts) timeout i fn
  when (g_verbose opts) $
    putStrLn ""
  return r

---- utilities -----------------------------------------------------

stdinIfNull :: [String] -> IO [String]
stdinIfNull fs =
  case fs of
    []  -> getContents >>= return . lines
    _   -> return fs
