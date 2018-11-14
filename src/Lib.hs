{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified GitHub as GH
import qualified Data.ByteString as B
import qualified Data.Vector as V
import GHC.Generics
import Data.Time

{-
getAuth :: B.ByteString -> GH.Auth
getAuth token = GH.OAuth token

getToken :: IO B.ByteString
getToken = do
  token <- B.readFile "./githubToken.txt"
  return token
-}

data CommitInfo = CommitInfo {
       timeOfCommit :: UTCTime
     , totalLines :: Int
     , newLines :: Int
     , delLines :: Int
     } deriving (Generic, Show)


--TODO: Need to request each commit to get the commit information
--TODO: Still need to sort out authentication


getCommit :: IO (Either GH.Error GH.Commit)
getCommit = do
  response <- GH.executeRequest' $
              GH.commitR "jdcarbeck" "GIT" "d4ed25f7715f704025a5dc8d1546cf291f29962c"
  return response
--This function is the organisation and repo that is to be analyse
getAllCommits :: IO (Either GH.Error (V.Vector GH.Commit))
getAllCommits = do
  response <- GH.executeRequest' $
              GH.commitsForR "jdcarbeck" "GIT" GH.FetchAll
  return response

func = do
  response <- getAllCommits
  case response of
    (Left error) -> return []
    (Right commitsList) -> return $ (V.toList commitsList)


mkCommitInfo :: GH.Commit -> CommitInfo
mkCommitInfo commit = CommitInfo { timeOfCommit = getTimeOfCommit commit
                                , totalLines = getCommitTotal $ GH.commitStats commit
                                , newLines = getCommitAdd $ GH.commitStats commit
                                , delLines = getCommitSub $ GH.commitStats commit
                                }

getInfoFromCommits :: [GH.Commit] -> [CommitInfo]
getInfoFromCommits [] = []
getInfoFromCommits (x:[]) = (mkCommitInfo x) : []
getInfoFromCommits (x:xs) = (mkCommitInfo x) : (getInfoFromCommits xs)

--This function will take a commit and request the the specific commit to get the
--information that pertains to the stat
-- commitStats :: GH.Commit -> [Int]


getCommitAdd :: Maybe GH.Stats -> Int
getCommitAdd stats =
  case stats of
    (Nothing) -> 0
    (Just stats) -> GH.statsAdditions stats

getCommitSub :: Maybe GH.Stats -> Int
getCommitSub stats =
  case stats of
    (Nothing) -> 0
    (Just stats) -> GH.statsDeletions stats

getCommitTotal :: Maybe GH.Stats -> Int
getCommitTotal stats =
  case stats of
    (Nothing) -> 0
    (Just stats) -> GH.statsTotal stats

getTimeOfCommit :: GH.Commit -> UTCTime
getTimeOfCommit commit =  GH.gitUserDate (GH.gitCommitAuthor $
                                          GH.commitGitCommit commit)
