# GIT-IO
This project looks at commits for a given repository and gives a graph displaying
the code churn and efficiency over time.
## Setup before running
This project requires the addition of `Auth.hs` in the src directory. This file
should contain the following code, where the two strings are replaced with your
username and password for github.com:
 ```Haskell
 module Auth where

 import qualified GitHub as GH

 getAuth :: GH.Auth
 getAuth = GH.BasicAuth "USERNAME" "PASSWORD"
 ```
