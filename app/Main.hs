{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified GitHub as GH

main :: IO [CommitInfo]
main = func
