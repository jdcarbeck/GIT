{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Requests where

import qualified GitHub as GH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector as V
import qualified Data.Text as T

import GHC.Generics
import Data.Aeson
import Data.Time
import Auth

data CommitData = CommitData {
      user :: T.Text
    , repo :: T.Text
    , commits :: [CommitInfo]
    } deriving (Generic, Show)

instance ToJSON CommitData

data CommitInfo = CommitInfo {
       timeOfCommit :: UTCTime
     , totalLines :: Int
     , newLines :: Int
     , delLines :: Int
     } deriving (Generic, Show)

instance ToJSON CommitInfo

requestGitHubStats :: String -> String -> IO (Either GH.Error CommitData)
requestGitHubStats ownerStr repoStr = do
  let owner = GH.mkName owner (T.pack(ownerStr))
  let repo = GH.mkName repo (T.pack(repoStr))
  response <- getAllCommits owner repo
  case response of
    (Left error) -> return $ Left error
    (Right commitsList) -> do
      let list = (V.toList commitsList)
      commitInfo <- getInfoFromCommits owner repo list
      let commitData = CommitData { user = (T.pack(ownerStr))
                                  , repo = (T.pack(repoStr))
                                  , commits = commitInfo }
      return $ Right commitData

getAllCommits :: GH.Name GH.Owner -> GH.Name GH.Repo -> IO (Either GH.Error (V.Vector GH.Commit))
getAllCommits owner repo = do
  response <- GH.executeRequest getAuth $
              GH.commitsForR owner repo GH.FetchAll
  return response

getInfoFromCommits :: GH.Name GH.Owner -> GH.Name GH.Repo -> [GH.Commit] -> IO [CommitInfo]
getInfoFromCommits owner repo ([]) = return []
getInfoFromCommits owner repo (x:[]) = do
  logRequestToConsole x
  stats <- commitStats owner repo x
  case (mkCommitInfo x stats) of
    (Nothing) -> return []
    (Just commitInfo) -> return $ commitInfo : []
getInfoFromCommits owner repo (x:xs) = do
  logRequestToConsole x
  stats <- commitStats owner repo x
  list <- getInfoFromCommits owner repo xs
  case (mkCommitInfo x stats) of
    (Nothing) -> return list
    (Just commitInfo) -> return $ commitInfo : list

logRequestToConsole :: GH.Commit -> IO ()
logRequestToConsole commit = do
  putStrLn $ "Request for commit: " ++ (show $ GH.untagName (GH.commitSha commit))

mkCommitInfo :: GH.Commit -> [Int] -> Maybe CommitInfo
mkCommitInfo commit stats =
  case (stats!!1 == 0) of
    (True) -> Nothing
    (False) -> Just CommitInfo { timeOfCommit = getTimeOfCommit commit
                          , newLines = (stats!!0)
                          , totalLines = (stats!!1)
                          , delLines = (stats!!2)}

commitStats :: GH.Name GH.Owner -> GH.Name GH.Repo -> GH.Commit -> IO [Int]
commitStats owner repo commit = do
  response <- GH.executeRequest getAuth $ GH.commitR owner repo (GH.commitSha commit)
  case response of
    (Left error) -> return []
    (Right commit) -> return $ getStats (GH.commitStats commit)

getStats :: Maybe GH.Stats -> [Int]
getStats stats =
  case stats of
     (Nothing) -> (0:0:0:[])
     (Just stats) -> ((GH.statsAdditions stats):(GH.statsTotal stats):(GH.statsDeletions stats):[])

getTimeOfCommit :: GH.Commit -> UTCTime
getTimeOfCommit commit =  GH.gitUserDate (GH.gitCommitAuthor $
                                          GH.commitGitCommit commit)
