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
getAllCommits :: IO (Either GH.Error (V.Vector GH.Commit))
getAllCommits = do
  response <- GH.executeRequest' {-(getAuth token)-} $
           GH.commitsForR "jdcarbeck" "guidebot" GH.FetchAll
  return response

func = do
  response <- getAllCommits
  case response of
    (Left error) -> return []
    (Right commitsList) -> return $ getInfoFromCommits (V.toList commitsList)


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

getCommitAdd :: Maybe GH.Stats -> Int
getCommitAdd stats =
  case stats of
    (Nothing) -> 1
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
