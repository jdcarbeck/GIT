#!/bin/sh
printf "\033[1;31mRunning Commit Visualisation 2018 John Carbeck\033[0m\n"
if [ "$#" -eq 2 ]
then
  cd data
  touch "$1_$2_data.json"
  cd ../
  cd GIT-IO
  stack build
  printf "\033[0;36mGreenGetting all commits for: \033[0;32mgithub.com/$1/$2\033[0m...\n"
  stack exec -- GIT-IO-exe -i $1 $2
  printf "\033[0;34mDone\033[0m\n"
  cd ../
fi
python -mwebbrowser http://localhost:8080
printf "\033[0;34mServer Started at http://localhost:8080 ctrl c to quit\033[0m\n"
python -m SimpleHTTPServer 8080
