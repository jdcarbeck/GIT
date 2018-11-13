{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified GitHub as GH

main :: IO ()
main = do
  response <- getAllReposFromOrg
  case response of
    (Left error) -> print "Error!\n"
    (Right resp) -> print $ GH.repoSize (firstOfResponse resp)
