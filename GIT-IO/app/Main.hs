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
      response <- requestGitHubStats (args!!1) (args!!2)
      case response of
        (Left error) -> putStrLn $ show error
        (Right response) -> do
          encodeFile (getFilePath (args!!1) (args!!2)) response
      return response


getFilePath :: String -> String -> FilePath
getFilePath owner repo = "../data/" ++ owner ++ "_" ++ repo ++ "_data.json"
