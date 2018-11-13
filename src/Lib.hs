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

firstOfResponse :: V.Vector a -> a
firstOfResponse x = head $ V.toList x
