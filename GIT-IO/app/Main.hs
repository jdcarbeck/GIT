{-# LANGUAGE OverloadedStrings #-}

module Main where

import Requests
import System.Environment
import System.IO
import qualified GitHub as GH
import qualified Data.Text as T
import Data.Aeson

main = do
  args <- getArgs
  let argCount = length args
  case (argCount == 3) of
    (True) -> do
      response <- requestGitHubStats (args!!2) (args!!1 == "org")
      case response of
        (Left error) -> putStrLn $ show error
        (Right response) -> do
          encodeFile (getFilePath (args!!2)) response
      return response


getFilePath :: String -> FilePath
getFilePath owner = "../data/" ++ owner ++ "_data.json"
