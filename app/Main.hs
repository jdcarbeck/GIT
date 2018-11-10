{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Git


main :: IO(Maybe a)
main = do
  response <- query
  return Nothing
{-
TODO: Need to make requests to get
-}
