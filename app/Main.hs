{-# LANGUAGE OverloadedStrings #-}

module Main where

import Requests
import System.Environment
import qualified GitHub as GH
import qualified Data.Text as T

main :: IO [CommitInfo]
main = do
  args <- getArgs
  let argCount = length args
  case (argCount == 3) of
    (True) -> do
      response <- requestGitHubStats (T.pack (args!!1)) (T.pack (args!!2))
      return response
    (False) -> return []


{-
  What I want to do before moving on to d3js
   -TODO: get command line arguments to pass through shell script for running
   -TODO: fix error handelling so the execution only happens on repos of appropriate six
   -TODO: better passing of errors through functions
   -TODO: output to JSON file that can be read to d3.js
-}
