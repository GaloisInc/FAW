{-# OPTIONS_GHC -Wno-missing-signatures -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module MongoExample where

-- based on commented example in Database.MongoDB


import Control.Monad.Trans (liftIO)

-- package: mongoDB
import Database.MongoDB


main2 :: IO ()
main2 = runInDB "baseball" action1

run_bb = runInDB "baseball"

runInDB db action =
  do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master db action
  close pipe
  return e

action1 :: Action IO ()
action1 =
  do
  clearTeams
  _ <- insertTeams
  allTeams >>= printDocsA "All Teams"
  nationalLeagueTeams >>= printDocsA "National League Teams"
  newYorkTeams >>= printDocsA "New York Teams"

clearTeams :: Action IO ()
clearTeams = delete (select [] "team")

insertTeams :: Action IO [Value]
insertTeams = insertMany "team"
  [
    ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
    ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
    ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
    ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"]
  ]

allTeams :: Action IO [Document]
allTeams = rest =<< find (select [] "team") {sort = ["home.city" =: 1]}

nationalLeagueTeams :: Action IO [Document]
nationalLeagueTeams = rest =<< find (select ["league" =: "National"] "team")

newYorkTeams :: Action IO [Document]
newYorkTeams = rest =<< find (select ["home.state" =: "NY"] "team") {project = ["name" =: 1, "league" =: 1]}

printDocsA :: String -> [Document] -> Action IO ()
printDocsA title docs =
  liftIO $
    do
    putStrLn title
    mapM_ printDoc docs
    putStrLn ""

printDoc :: Document -> IO ()
printDoc doc =
  print $ exclude ["_id"] doc
