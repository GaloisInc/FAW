{-# LANGUAGE OverloadedStrings #-}
-- FIXME: remove need for above

module Database where

-- package: mongoDB
import Database.MongoDB

-- local:
import Types

---- our 'schema' and DB -----------------------------------------------------

-- littleGovdocs  = "littlegovdocs"  :: Database

-- rawInvocations :: Collection
-- rawInvocations = "rawinvocations" -- holds [Invocation]

---- queries: abstractions ---------------------------------------------------

mySelectAndProject :: Globals -> Selector -> Projector -> IO [Document]
mySelectAndProject o s p =
  runInDB o
  $ find (select s (g_collection o)){project= p}
    >>= rest


---- running -----------------------------------------------------------------

runInDB :: Globals -> Action IO b -> IO b
runInDB o action =
  do
  pipe <- connect (g_server o)
  e <- access pipe master (g_database o) action
  close pipe
  return e
