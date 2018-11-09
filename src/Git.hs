{-# LANGUAGE OverloadedStrings #-}

module Git where

-- import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Conduit

httpWithUserAgent :: IO String
httpWithUserAgent = do
  r <- parseUrlThrow "https://api.github.com/users/jdcarbeck"
  let request = r {requestHeaders = [("User-Agent", "jdcarbeck")]}
  manager <- newManager tlsManagerSettings
  res <- httpLbs request manager
  return . show . responseBody $ res

--TODO:get all of the github users with reposatories greater then 10
