{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Requests where

import qualified GitHub as GH
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Text
import GHC.Generics
import Data.Time
import Auth


--These are set by the function requestGitHubStats

data CommitInfo = CommitInfo {
       timeOfCommit :: UTCTime
     , totalLines :: Int
     , newLines :: Int
     , delLines :: Int
     } deriving (Generic, Show)

requestGitHubStats :: Text -> Text -> IO [CommitInfo]
requestGitHubStats ownerStr repoStr = do
  let owner = GH.mkName owner ownerStr
  let repo = GH.mkName repo repoStr
  response <- getAllCommits owner repo
  case response of
    (Left error) -> return []
    (Right commitsList) -> do
      commitInfo <- getInfoFromCommits owner repo (V.toList commitsList)
      return commitInfo

getAllCommits :: GH.Name GH.Owner -> GH.Name GH.Repo -> IO (Either GH.Error (V.Vector GH.Commit))
getAllCommits owner repo = do
  response <- GH.executeRequest getAuth $
              GH.commitsForR owner repo GH.FetchAll
  return response

getInfoFromCommits :: GH.Name GH.Owner -> GH.Name GH.Repo -> [GH.Commit] -> IO [CommitInfo]
getInfoFromCommits owner repo (x:[]) = do
  logRequestToConsole x
  stats <- commitStats owner repo x
  return $ (mkCommitInfo x stats) : []
getInfoFromCommits owner repo (x:xs) = do
  logRequestToConsole x
  stats <- commitStats owner repo x
  list <- getInfoFromCommits  owner repo xs
  return $ (mkCommitInfo x stats) : list

logRequestToConsole :: GH.Commit -> IO ()
logRequestToConsole commit = do
  putStrLn $ "Request for commit: " ++ (show $ GH.untagName (GH.commitSha commit))

--TODO: need to implement error handling error where stat is just an empty list
mkCommitInfo :: GH.Commit -> [Int] -> CommitInfo
mkCommitInfo commit stats = CommitInfo { timeOfCommit = getTimeOfCommit commit
                                        , newLines = (stats!!0)
                                        , totalLines = (stats!!1)
                                        , delLines = (stats!!2)
                                        }

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
     (Just stats) -> ((GH.statsAdditions stats):(GH.statsTotal stats):(GH.statsTotal stats):[])

getTimeOfCommit :: GH.Commit -> UTCTime
getTimeOfCommit commit =  GH.gitUserDate (GH.gitCommitAuthor $
                                          GH.commitGitCommit commit)