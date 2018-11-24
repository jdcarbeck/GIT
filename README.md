# GIT-IO
![alt text](https://raw.githubusercontent.com/jdcarbeck/git/master/img.png)
## Setup before running
This project requires the addition of `Auth.hs` in the `./GIT-IO/src` directory. This file
should contain the following code, where the two strings are replaced with your
username and password for github.com:
 ```Haskell
 {-# LANGUAGE OverloadedStrings #-}
 module Auth where

 import qualified GitHub as GH

 username = "username"
 password = "password"

 getAuth :: GH.Auth
 getAuth = GH.BasicAuth username password
 ```

 ## Running the project
 To run the project on a user `./run.sh u {USERNAME}` or for an organisation `./run.sh org {ORG}`
 To run the project with the data previously collected run `./run.sh`
