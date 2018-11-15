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
      response <- requestGitHubStats (T.pack (args!!1)) (T.pack (args!!2))
      case response of
        (Left error) -> putStrLn $ show error
        (Right response) -> do
          encodeFile (getFilePath (args!!2)) response
      return response


getFilePath :: String -> FilePath
getFilePath repo = "../data/" ++ repo ++ "_data.json"
  --TODO: output to JSON file that can be read to d3.js
