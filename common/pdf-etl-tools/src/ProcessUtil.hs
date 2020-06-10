{-# LANGUAGE OverloadedStrings #-}

module ProcessUtil where

import System.Exit
import System.Timeout

-- package bytestring:
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

-- package: text
-- NB: the strict versions here being referenced:
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T

-- package: unliftio
import            UnliftIO.Temporary (withTempFile)

-- package: typed-process
import            System.Process.Typed
   -- "replaces" base System.Process

-- local
import            TempFiles

---- a dip into the 'textual' morass -----------------------------------------

-- typed-processes uses lazy 'ByteString'
-- BSON uses strict 'Text'
-- it seems that here is where we want to strictify:

bytesToText :: BL.ByteString -> T.Text
bytesToText =
  T.decodeUtf8With T.lenientDecode . B.concat . BL.toChunks


---- small abstraction over readProcess --------------------------------------

{-
-- the version that uses standard System.Process:
readProcessWithExitCodeOrTimeout
    :: FilePath                 -- ^ Filename of the executable
    -> [String]                 -- ^ any arguments
    -> Int                      -- ^ timeout in microseconds
    -> String                   -- ^ standard input
    -> IO (Maybe (ExitCode,String,String)) -- ^ exitcode, stdout, stderr
readProcessWithExitCodeOrTimeout cmd args microsecs input =
  timeout microsecs
     (readProcessWithExitCode cmd args input)
-}

readProcessWithExitCodeOrTimeout
    :: FilePath                 -- ^ Filename of the executable
    -> [String]                 -- ^ any arguments
    -> Maybe Int                -- ^ timeout in microseconds (or no timeout)
    -> BL.ByteString            -- ^ standard input
    -> IO (Maybe ExitCode, T.Text, T.Text)
          -- ^ (exitcode, stdout, stderr)
readProcessWithExitCodeOrTimeout cmd args mMicrosecs input =
  getTempDirName >>= \tdir->
  withTempFile tdir "x-stdout" $ \stdout_fp stdout_h->
  withTempFile tdir "x-stderr" $ \stderr_fp stderr_h->
    do
    let pc = setStdin (byteStringInput input)
           $ setStdout (useHandleClose stdout_h)
           $ setStderr (useHandleClose stderr_h)
           $ proc cmd args

    me <- case mMicrosecs of
            Just ms -> timeout ms (runProcess pc)
            Nothing -> Just <$> runProcess pc

    -- print (stdout_fp,stderr_fp)
    stdout_text <- readAndTruncate stdout_fp
    stderr_text <- readAndTruncate stderr_fp
    -- print (T.length stdout_text, T.length stderr_text)
    return (me, stdout_text, stderr_text)

  where
  maxLength = 1000000 -- 1 million max bytes

  readAndTruncate filepath =
    do
    bs <- BL.readFile filepath
    let len = BL.length bs
        bs' = if len < maxLength then
                bs
              else
                BL.take maxLength bs `BL.append` "\npdf-etl-tool: ERROR: DATA-TRUNCATED\n"

    return (bytesToText bs')

  -- FIXME[F2]: this may cause the last 'good' line to be unparseable
  -- FIXME[F3]: deal in lines not chars??
