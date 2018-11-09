{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

import Git

main :: IO()
main = do
  httpWithUserAgent >>= putStrLn
{-
TODO: Need to make requests to get
-}
