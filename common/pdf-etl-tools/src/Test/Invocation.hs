{-# OPTIONS_GHC -Wno-missing-signatures -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Invocation where

import Data.Typeable

-- package: bson
import Data.Bson

-- package: bson-mapping:
import Data.Bson.Mapping

-- package pretty-show
import qualified Text.Show.Pretty as PS

-- local:
import Invocation
import InvocationTypes

---- test data ---------------------------------------------------------------

shortTimeoutScale = Just 5

invokerList :: [Invoker_Src]
invokerList =
  [ Invoker
      [Str "pdfinfo", InputFile]
      (Just 3)
      "poppler-0.62.0"
      "pdfinfo"
  , Invoker
      [Str "pdfinfo", Str "-meta", InputFile]
      (Just 3)
      "poppler-0.62.0"
      "pdfinfo-meta"
  , Invoker
      [Str "pdfinfo", Str "-struct", InputFile]
      (Just 18)
      "poppler-0.62.0"
      "pdfinfo-struct"
  , Invoker
      [Str "pdftocairo", Str "-pdf", InputFile, TmpFN "pdftocairo.pdf"]
      (Just 18)
      "poppler-0.62.0"
      "pdftocairo-pdf"
  , Invoker
      [Str "qpdf", Str "--check", InputFile]
      (Just 18)
      "8.0.2"
      "qpdf-check"
  , Invoker
      [Str "python", Str "../lib/pdfid_v0_2_5/pdfid.py", Str "-e", InputFile]
      (Just 18)
         -- (Just 3) was timing out!
         --   because this scans whole file?
      "pdfid_v0_2_5"
         -- FIXME: a bit ugly in that we have a relative path here!
      "pdfid"
  , Invoker
      [Str "caradoc", Str "stats", InputFile]
      (Just 18)
      "caradoc-0.3"
      "caradoc-stats"
  , Invoker
      [Str "caradoc", Str "stats", Str "--strict", InputFile]
      (Just 18)
      "caradoc-0.3"
      "caradoc-strict"
  , Invoker
      -- this simply segfaults; used for testing!
      [Str "./c-src/die", InputFile]
      (Just 3)
      ""
      "die-test"
  ]


inv1 = Invocation i1 "filename" (Timeout 3 "\n" "\n")


i1 = (invokerList!!0){exec=["c","a1"]}
i1' = Invoker_Export i1{exec=([] :: [String])}

t1  = invocationToDoc inv1
t1' = docToInvocation $ invocationToDoc inv1

p1 = PS.ppShow invokerList

---- test code ---------------------------------------------------------------

testVal :: Val a => a -> Bool
testVal a = case cast' . val $ a of
    Nothing -> False
    Just a' -> a == a'

---- explore -----------------------------------------------------------------

data Post = Post { author :: String
                 , content :: String
                 , votes :: Int
                 }
            deriving (Show, Read, Eq, Ord, Typeable)

$(deriveBson ''Post)

test2 :: IO ()
test2 =
  do
  let post = Post "francesco" "lorem ipsum" 5
  (fromBson (toBson post) :: IO Post) >>= print
  print $ toBson post
  print $ g' post

g' p = $(selectFields ['author, 'votes]) p
