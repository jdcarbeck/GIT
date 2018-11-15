{-# LANGUAGE OverloadedStrings #-}

module Main where

import Requests
import System.Environment
import qualified GitHub as GH
import qualified Data.Text as T

main = do
  args <- getArgs
  let argCount = length args
  case (argCount == 3) of
    (True) -> do
      response <- requestGitHubStats (T.pack (args!!1)) (T.pack (args!!2))
      case response of
        (Left error) -> putStrLn $ show error
        (Right response) -> putStrLn $ show response
      return response



{-
  What I want to do before moving on to d3js
   -TODO: output to JSON file that can be read to d3.js
-}
