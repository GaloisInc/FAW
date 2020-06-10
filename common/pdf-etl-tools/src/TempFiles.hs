module TempFiles
  ( createTempDir
  , getTempFileName
  , removeTempDir
  )
  where

import System.Directory (createDirectoryIfMissing,removePathForcibly)
import System.Posix.Process (getProcessID)

getTempFileName :: String -> IO String
getTempFileName template =
  do
  tempDir <- getTempDirName
  return $ concat [tempDir,"/",template]

-- | createTempDir - must be called before running (certain) invokers.
createTempDir :: IO ()
createTempDir =
  do
  tempDir <- getTempDirName
  createDirectoryIfMissing True tempDir

-- e.g., to be called on INT signal (or ...?)
removeTempDir :: IO ()
removeTempDir =
  do
  tempDir <- getTempDirName
  removePathForcibly tempDir



getTempDirName :: IO String
getTempDirName =
  do
  pid <- getProcessID
  return (concat [tempDir1,"/pid-",show pid])

  where
  tempDir1 :: String
  tempDir1 = "/tmp/pdf-etl-tool"  -- should work for linux & mac

-- FIXME[C2]: unfortunate we to bake this in (rather than calling lib functions in Unliftio.Temporary).  Rework to remove this.
