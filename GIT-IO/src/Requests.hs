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
    , commits :: [CommitInfo]
    } deriving (Generic, Show)

instance ToJSON CommitData

data CommitInfo = CommitInfo {
       timeOfCommit :: UTCTime
     } deriving (Generic, Show)

instance ToJSON CommitInfo

requestGitHubStats :: String -> Bool -> IO (Either GH.Error CommitData)
requestGitHubStats ownerStr isOrg = do
  case isOrg of
    (True) -> do
      let organization = GH.mkName organization (T.pack(ownerStr))
      response <- getOrgRepos organization
      case response of
        (Left error) -> return $ Left error
        (Right repos) -> do
          putStrLn "here"
          commitData <- commitDataFromRepos (V.toList repos) ownerStr
          return $ Right commitData
    (False) -> do
      let owner = GH.mkName owner (T.pack(ownerStr))
      response <- getUserRepos owner
      case response of
        (Left error) -> return $ Left error
        (Right repos) -> do
          commitData <- commitDataFromRepos (V.toList repos) ownerStr
          return $ Right commitData

commitDataFromRepos :: [GH.Repo] -> String -> IO CommitData
commitDataFromRepos repos owner = do
  commitsFromRepos <- getInfoFromRepoList repos owner
  return CommitData { user = T.pack(owner)
                    , commits = commitsFromRepos}

getInfoFromRepoList :: [GH.Repo] -> String -> IO [CommitInfo]
getInfoFromRepoList ([]) ownerStr = return []
getInfoFromRepoList (x:[]) ownerStr = do
  let owner = GH.mkName owner (T.pack ownerStr)
  commits <- getRepoCommits owner (GH.repoName x)
  case commits of
    (Left error) -> return []
    (Right commits) -> return (getInfoFromCommits (V.toList commits))
getInfoFromRepoList (x:xs) ownerStr = do
  let owner = GH.mkName owner (T.pack(ownerStr))
  list <- getInfoFromRepoList xs ownerStr
  commits <- getRepoCommits owner (GH.repoName x)
  case commits of
    (Left error) -> return list
    (Right commits) -> return ((getInfoFromCommits (V.toList commits)) ++ list)

getInfoFromCommits :: [GH.Commit] -> [CommitInfo]
getInfoFromCommits [] = []
getInfoFromCommits (x:[]) = ((mkCommitInfo x):[])
getInfoFromCommits (x:xs) = ((mkCommitInfo x):(getInfoFromCommits xs))

getUserRepos :: GH.Name GH.Owner -> IO (Either GH.Error (V.Vector GH.Repo))
getUserRepos owner = do
  response <- GH.executeRequest getAuth $
              GH.userReposR owner GH.RepoPublicityPublic GH.FetchAll
  return response

getOrgRepos :: GH.Name GH.Organization -> IO (Either GH.Error (V.Vector GH.Repo))
getOrgRepos org = do
  response <- GH.executeRequest getAuth $
              GH.organizationReposR org GH.RepoPublicityPublic GH.FetchAll
  return response

getRepoCommits :: GH.Name GH.Owner -> GH.Name GH.Repo -> IO (Either GH.Error (V.Vector GH.Commit))
getRepoCommits owner repo = do
  response <- GH.executeRequest getAuth $
              GH.commitsForR owner repo GH.FetchAll
  return response

mkCommitInfo :: GH.Commit -> CommitInfo
mkCommitInfo commit = CommitInfo { timeOfCommit = getTimeOfCommit commit}

getTimeOfCommit :: GH.Commit -> UTCTime
getTimeOfCommit commit =  GH.gitUserDate (GH.gitCommitAuthor $
                                          GH.commitGitCommit commit)
