{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import qualified GitHub as GH
import qualified Data.ByteString as B
import qualified Data.Vector as V
import GHC.Generics
import Data.Time
import Auth

{-
getAuth :: B.ByteString -> GH.Auth
getAuth token = GH.OAuth token

getToken :: IO B.ByteString
getToken = do
  token <- B.readFile "./githubToken.txt"
  return token
-}

--These are the paramerters that will need to change to query different results
owner = "howtographql"
repo = "howtographql"

data CommitInfo = CommitInfo {
       timeOfCommit :: UTCTime
     , totalLines :: Int
     , newLines :: Int
     , delLines :: Int
     } deriving (Generic, Show)

func = do
  response <- getAllCommits
  case response of
    (Left error) -> return []
    (Right commitsList) -> do
      commitInfo <- getInfoFromCommits (V.toList commitsList)
      return commitInfo

getAllCommits :: IO (Either GH.Error (V.Vector GH.Commit))
getAllCommits = do
  response <- GH.executeRequest getAuth $
              GH.commitsForR owner repo GH.FetchAll
  return response

getInfoFromCommits :: [GH.Commit] -> IO [CommitInfo]
getInfoFromCommits (x:[]) = do
  logToConsole x
  stats <- commitStats x
  return $ (mkCommitInfo x stats) : []
getInfoFromCommits (x:xs) = do
  logToConsole x
  stats <- commitStats x
  list <- getInfoFromCommits xs
  return $ (mkCommitInfo x stats) : list

logToConsole :: GH.Commit -> IO ()
logToConsole commit = do
  putStrLn $ "Request for commit: " ++ (show $ GH.untagName (GH.commitSha commit))


mkCommitInfo :: GH.Commit -> [Int] -> CommitInfo
mkCommitInfo commit stats = CommitInfo { timeOfCommit = getTimeOfCommit commit
                                        , newLines = (stats!!0)
                                        , totalLines = (stats!!1)
                                        , delLines = (stats!!2)
                                        }

commitStats :: GH.Commit -> IO [Int]
commitStats commit = do
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
