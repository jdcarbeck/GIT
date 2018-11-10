# GIT
This project interrogates the Github V3 REST api

## Running the project
First make the changes to the Git.hs file as specified by the setup section below.
The project can then be run using `./run.sh`

### Setup
The project must contain a githubToken.txt in the source dir. Within this file
contains `token YOURTOKEN` where YOURTOKEN is the token generated through the
Github account by going to settings/Developer settings/Personal access tokens

The userAgent at the top of Git.hs must also be changed to the username of that
who is interrogating the api
