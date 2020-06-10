module TempFiles
  ( createTempDir
  , getTempFileName
  , getTempDirName
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

-- | createTempDir - must be called before writing any 'getTempFileName's
createTempDir :: IO ()
createTempDir =
  do
  tempDir <- getTempDirName
  createDirectoryIfMissing True tempDir

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
