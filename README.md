# GIT-IO
This project looks at commits for a given repository and gives a graph displaying
the code churn and efficiency over time.
## Setup before running
This project requires the addition of `Auth.hs` in the `./src` directory. This file
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
 To run the project simple run `./run.sh {USER or ORG} {REPO}`
 where the user or org is the owner of the repo and the repo is the name of the
 repo that the commit information needs to be collected on
