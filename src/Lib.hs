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

mkCommitInfo :: UTCTime -> Int -> Int -> Int -> CommitInfo
mkCommitInfo t tl nl dl = CommitInfo { timeOfCommit=t, totalLines=tl
                                     , newLines=nl, delLines=dl }

toListFromResponse :: (Either GH.Error (V.Vector a)) -> [a]
toListFromResponse possibleVector =
  case possibleVector of
    (Left error) -> []
    (Right vector) -> V.toList vector

getInfoFromCommits :: [GH.Commit] -> Maybe [CommitInfo]
getInfoFromCommits [] = Nothing

getCommitAdd :: GH.Commit -> Maybe Int
getCommitAdd possibleCommit =
  case (GH.commitStats possibleCommit) of
    (Nothing) -> Nothing
    (Just addStats) -> Just $ GH.statsAdditions addStats

getCommitSub :: GH.Commit -> Maybe Int
getCommitSub possibleCommit =
  case (GH.commitStats possibleCommit) of
    (Nothing) -> Nothing
    (Just subStats) -> Just $ GH.statsDeletions subStats

-- getCommitTotal ::
--
-- getTimeOfCommit ::
