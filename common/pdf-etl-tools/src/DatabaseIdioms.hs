{-# OPTIONS_GHC -Wno-missing-signatures -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- without both of these, the Hs syntax for docs/queries would not compile!

module DatabaseIdioms where

import           Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict as M
import           System.FilePath

-- package: bson:
import           Data.Bson

-- package: mongoDB:
-- import           Database.MongoDB

-- local modules:
import           Database
import           Invocation
import           InvocationTypes
import           Types

---- various decision procedures ---------------------------------------------

-- | DP - Decision Procedure
type DP         = DP' Confidence
type DP' a      = M.Map InvokerName Result -> a
type Confidence = Int -- [0..100]


all0_DP :: DP
all0_DP a = ratioToConfidence (length zs) (length rs)
  where
  zs = filter (==0) rs  -- zeros
  rs = catMaybes $ map getExitCode $ M.elems a

any0_DP :: DP
any0_DP a = ratioToConfidence (if one then 1 else 0) 1
  where
  one = any (==0) rs    -- is anything zero?
  rs = catMaybes $ map getExitCode $ M.elems a


ratioToConfidence x m = 100 * x `div` m

getExitCode :: Result -> Maybe Int
getExitCode (GoodResult c _ _ _) = Just c
getExitCode _                    = Nothing

-- now for some 'ad hoc' decision procedures

a_DP :: DP
a_DP a = ratioToConfidence (length zs) (length rs)
  where
  zs = filter (==0) rs  -- zeros
  rs = catMaybes $ map getExitCode $ M.elems a
  -- FIXME: implement: make different!
   -- allow qpdf to have exit code 6 : or do as a transform!


{-
ideas
  - no tool crashes

tools/lib
  - is exitcode a segfault?

-}

---- exploring classification ------------------------------------------------

decideGovdocsFile :: Globals -> FilePath -> IO ()
decideGovdocsFile o f = decideFile o ("/media/data/raw/govdocs/" </> f)

decideFile :: Globals -> FilePath -> IO ()
decideFile o f =
  do
  putStrLn $ "file: " ++ f
  forM_ [(all0_DP,"all","exit codes of all tools are good")
        ,(any0_DP,"any","exit code of one tool is good")
        -- ,(a_DP   ,"a"  ,"qpdf can return xx, other tools all good")
        ]
    (decideVerbose o f)
  putStrLn ""


decideVerbose :: Globals -> FilePath -> (DP,String,String) -> IO ()
decideVerbose o f (dpF,dpName,dpDescription) =
  do
  r <- decide o dpF f

  putStrLn $ "decision procedure '"++dpName++"' ("++dpDescription++"):"
  putStrLn $ " result = "++show r++"/100" -- FIXME: magic number

decide :: Globals -> DP -> FilePath -> IO Confidence
decide o decisionProcedure fp =
  do
  rs <- fileResults o fp
  return (decisionProcedure rs)

fileResults :: Globals -> FilePath -> IO (M.Map InvokerName Result)
fileResults o f =
  do
  -- FIXME:
  -- - do you want to specify desired invocations?
  -- - currently everything in list

  docs <- mySelectAndProject o ["file" =: f] []
  let n = length docs

  let rs =
        map ((\(Invocation i _ r)-> (invName i, r)) . docToInvocation)
            docs

  putStrLn $ "log: using results of " ++ show n ++ " invocations"

  -- FIXME:
  -- when (n < 6) $
  --   putStrLn "- missing invocations"
  -- when (n > 6) $
  --   putStrLn "- extra invocations"
  -- when (length (nub (map fst irs)) /= length irs) $
  --   putStrLn $ "- invocations are not unique: "++ show irs

  return $ M.fromList rs

---- runtime errors ----------------------------------------------------------

get_RuntimeErrors :: Globals -> IO [(InvokerName ,FilePath)]
get_RuntimeErrors o =
  do
  docs <- mySelectAndProject o ["result._cons" =: "RuntimeError"] []
  let is = map ((\(Invocation i f _)-> (invName i,f)) . docToInvocation) docs
  return is

---- etc ---------------------------------------------------------------------

-- | select just one set of invokers (by 'invName') and project one field
--   from the `Invocation` document.
selectInvokersAndProjectField ::
   (Val b) => Globals -> InvokerName -> Label -> IO [b]
selectInvokersAndProjectField o iname lbl =
  do
  docs <- mySelectAndProject o ["invoker.invName" =: iname]
                               ["_id" =: 0, lbl =: 1]
  mapM (Data.Bson.lookup lbl) docs

---- 'post-processing' queries -----------------------------------------------
{-
-- | docToInvocation - may fail.
--     PRECONDITION: the Document will match 'Invocation'
docToInvocation :: Document -> Invocation
docToInvocation d =
  case fromBson d of
    Just i  -> i
    Nothing -> error
                 "Cannot parse Document into Invocation (does the database have the 'v2' schema?)"

prInvocations ds = mapM_ (print . docToInvocation) ds
  -- use only when you haven't projected!
-}

prListM xs = mapM_ print xs
lengthM xs = return $ length xs
