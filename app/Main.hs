{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Git


main :: IO()
main = do
  response <- query
  print response
{-
TODO: Need to make requests to get
-}
