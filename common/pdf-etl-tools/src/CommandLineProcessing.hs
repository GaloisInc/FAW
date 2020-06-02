module CommandLineProcessing
  ( cmdLineParser
  , optionsToGlobals
  , parseConfigFile

  , AddStyle(..)
  , Options(..)
  , Command(..)
  , O_AddRaw(..)
  , O_Decisions(..)
  )
where

-- system:
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Semigroup ((<>))
import qualified Data.Text as T

-- package: mongoDB
import qualified Database.MongoDB as DB

-- optparse-applicative pkg:
import           Options.Applicative

-- local:
import           InvocationTypes
import           Types
import           Util(split,exitWithMessage)

---- types -------------------------------------------------------------------

-- | the command line options
data Options =
  Options
    { opt_database     :: T.Text
    , opt_collection   :: T.Text
    , opt_server       :: DB.Host -- shows/reads as "hostname[:port]"
    , opt_verbose      :: Verbose
    , opt_invokersCfg  :: FilePath
    }
  deriving (Eq,Show)


invokersCfgDefault :: FilePath
invokersCfgDefault = "invokers.cfg"

serverDefault :: DB.Host
serverDefault = DB.readHostPort "localhost:27017" -- mongo's default port


-- | the commands we can use on the command line
data Command
  = C_AddRaw O_AddRaw
  | C_ListInvokers
  | C_Retry
  | C_Clear        -- ^ clear the collection
  | C_Decisions O_Decisions
  deriving (Eq,Show)

data AddStyle = AS_Insert
              | AS_InsertIfAbsent
           -- | AS_Upsert      -- maybe later functionality?
  deriving (Eq,Show)

data O_AddRaw =
  O_AddRaw { ar_dry       :: Bool          -- ^ dry-run: no invoke, no db
           , ar_nodb      :: Bool          -- ^ invoke, no add to db
           , ar_addStyle  :: AddStyle
           , ar_invokers  :: [String]      -- ^ which invokers
           , ar_timeout   :: TimeoutOverride
           , ar_files     :: [FilePath]      -- ^ if null, read from stdin
           }
  deriving (Eq,Show)

data O_Decisions =
  O_Decisions { d_files    :: [FilePath]  -- ^ if no files, read from stdin
              }
  deriving (Eq,Show)


---- primary export ----------------------------------------------------------

cmdLineParser :: IO (Options, Command)
cmdLineParser = execParser allArgsI


---- defining argument list --------------------------------------------------

options_p :: Parser Options
options_p =
  Options
      <$> strOption
              ( long "database"
             <> short 'd'
             <> metavar "DATABASE"
             <> value (T.pack "govdocs")
             <> showDefault
             <> help "the mongo database name")
      <*> strOption
              ( long "collection"
             <> short 'c'
             <> metavar "COLLECTION"
             <> value (T.pack "unspecified")
             <> showDefault
             <> help "the collection name")
      <*> option (eitherReader DB.readHostPortM)
              ( long "server"
             <> short 's'
             <> metavar "HOSTNAME[:port]"
             <> value serverDefault
             <> help "mongo server (default 'localhost')")
      <*> switch
              ( long "verbose"
             <> short 'v'
             <> help "verbose output" )
      <*> strOption
              (long "invokersfile"
             <> short 'i'
             <> metavar "FILE"
             <> value invokersCfgDefault
             <> showDefault
             <> help "config file describing invokers")


commands :: Parser Command
commands =
  hsubparser
    ( command "add-raw"
        (info (C_AddRaw <$> options_addRaw)
              ( progDesc "add raw output from invoking pdf tool(s) to DATABASE COLLECTION" ))
   <> command "decisions"
        (info (C_Decisions <$> options_decisions)
              ( progDesc "show decisions for files" ))
   <> command "list-invokers"
        (info (pure C_ListInvokers)
              ( progDesc "List the available pdf tools one can invoke"))
   <> command "retry"
        (info (pure C_Retry)
              ( progDesc "retry invocation for any RunTimeErrors in collection"))
   <> command "clear"
        (info (pure C_Clear)
              ( progDesc "delete all docs in DATABASE : COLLECTION"))
    )

parseInvokerList :: ReadM [String]
parseInvokerList =
  eitherReader $
    \s-> case split ',' s of
           [] -> Left "invoker list cannot be empty string"
           ns -> validate ns
  where
  validate is = if any null is then
                  Left "null string not allowed in invoker list"
                else if length is > length (nub is) then
                  Left "duplicate invokers"
                else if length is > 1 && "ALL" `elem` is then
                  Left "'ALL' cannot be an element of invoker list of 2+ items"
                else
                  Right is
                -- NOTE: validity & interpretation of strings are checked later.

parseTimeoutOverride :: ReadM TimeoutOverride
parseTimeoutOverride =
  eitherReader $
    \s-> case s of
           "no-timeout"  -> Right TO_NoTimeout
           "no-override" -> Right TO_NoOverride
           _             -> case readM s of
                              Just n -> Right $ TO_Timeout n
                              _      -> Left "cannot parse timeout-override flag"

readM :: Read a => String -> Maybe a
readM s = case reads s of
             [(a,[])] -> Just a
             _        -> Nothing

options_addRaw :: Parser O_AddRaw
options_addRaw =
      O_AddRaw
  <$> switch
      ( long "dryrun"
     <> short 'd'
     <> help "dry run: explain actions" )
  <*> switch
      ( long "nodb"
     <> short 'n'
     <> help "no database update (but invoke tool)" )
  <*> flag AS_Insert AS_InsertIfAbsent
          ( long "absentonly"
         <> short 'a'
         <> help "only insert invocations that are absent from COLLECTION")
  <*> option parseInvokerList
        (metavar "INVOKERLIST"
         <> long "invokers"
         <> short 'i'
         <> help "comma separated list of invokers to apply or 'ALL'"
        )
  <*> option parseTimeoutOverride
      ( metavar "(SECONDS|no-timeout|no-override)"
        <> long "timeout"
        <> short 't'
        <> value TO_NoOverride
        <> help "timeout in secs" )
  <*> many (argument str
              (  metavar "FILES..."
               <> help
                  "pdf files to process (if empty, reads lines from stdin)"
              ))

options_decisions :: Parser O_Decisions
options_decisions =
      O_Decisions
      <$> many (argument str
              (  metavar "FILES..."
               <> help
                  "pdf files to process (if empty, reads lines from stdin)"
              ))

allArgs :: Parser (Options, Command)
allArgs  = liftA2 (,) options_p commands

allArgsI :: ParserInfo (Options, Command)
allArgsI = info (allArgs <**> helper)
             (  fullDesc
             <> progDesc "A tool to manage pdf tool invocation and the mongo database"
             )

---- further processing of CL options ----------------------------------------

optionsToGlobals :: Options -> IO Globals
optionsToGlobals o =
  do
  let fn = opt_invokersCfg o

  -- NOTE: the invoker list file must exist, even for options that don't require
  is <- parseConfigFile fn
  return $
    Globals { g_database   = opt_database o
            , g_collection = opt_collection o
            , g_server     = opt_server o
            , g_verbose    = opt_verbose o
            , g_invokerList= is
            }

parseConfigFile :: FilePath -> IO [Invoker_Src]
parseConfigFile fn =
  do
  putStrLn $ "reading invoker list from" ++ filename
  cs <- readFile fn
  let cs' = unlines $ filter (not . isComment) $ lines $ cs
  is <- case reads cs' of
          [(is,s)] | all isSpace s ->
              return is
          r                        ->
              exitWithMessage $ unlines $
                [ "cannot parse the invoker description list in" ++ filename
                , "  " ++ show cs'
                , "  " ++ show r
                ]

  unless (length (nub( map invName is)) == length is) $
    exitWithMessage $
      "duplicate invoker entries in" ++ filename
      -- FIXME[U2]: nice to elaborate.
  unless (length is > 0) $
    exitWithMessage $
      "empty invoker list in" ++ filename
  forM_ is $ \i->
    unless (validInvName (invName i)) $
      exitWithMessage $
        "invalid invName "++ show(invName i) ++" in" ++ filename

  return is

  where
  filename = " file '" ++ fn ++ "'"
  isComment = isPrefixOf "--" . dropWhile isSpace

  validInvName s =
    case s of
      ""    -> False
      "ALL" -> False
      _     -> all (/= ',') s
