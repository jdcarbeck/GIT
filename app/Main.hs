{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.HTTP.Simple

import Git

main :: IO()
main = do
  httpWithUserAgent >>= putStrLn
{-
TODO: Need to make requests to get
-}
