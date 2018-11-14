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

--This function is the organisation and repo that is to be analyse
getAllCommits :: IO (Either GH.Error (V.Vector GH.Repo))
getAllCommits = do
  response <- GH.executeRequest' {-(getAuth token)-} $
           GH.userReposR "jdcarbeck" GH.RepoPublicityAll GH.FetchAll
  return response

mkCommitInfo :: GH.Commit -> CommitInfo
mkCommitInfo commit = CommitInfo { timeOfCommit = getTimeOfCommit commit
                                , totalLines = getCommitTotal commit
                                , newLines = getCommitAdd commit
                                , delLines = getCommitSub commit
                                }

toListFromResponse :: (Either GH.Error (V.Vector a)) -> [a]
toListFromResponse possibleVector =
  case possibleVector of
    (Left error) -> []
    (Right vector) -> V.toList vector

getInfoFromCommits :: [GH.Commit] -> [CommitInfo]
getInfoFromCommits [] = []
getInfoFromCommits (x:[]) = (mkCommitInfo x) : []
getInfoFromCommits (x:xs) = (mkCommitInfo x) : (getInfoFromCommits xs)

getCommitAdd :: GH.Commit -> Int
getCommitAdd commit =
  case (GH.commitStats commit) of
    (Nothing) -> 0
    (Just addStats) -> GH.statsAdditions addStats

getCommitSub :: GH.Commit -> Int
getCommitSub commit =
  case (GH.commitStats commit) of
    (Nothing) -> 0
    (Just subStats) -> GH.statsDeletions subStats

getCommitTotal :: GH.Commit -> Int
getCommitTotal commit =
  case (GH.commitStats commit) of
    (Nothing) -> 0
    (Just totalStats) -> GH.statsTotal totalStats

getTimeOfCommit :: GH.Commit -> UTCTime
getTimeOfCommit commit =  GH.gitUserDate (GH.gitCommitAuthor $
                                          GH.commitGitCommit commit)
