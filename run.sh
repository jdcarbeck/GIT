#!/bin/sh
printf "\033[1;31mRunning Commit Visualisation\033[0;31m\n2018 John Carbeck\033[0m\n"
if [ "$#" -eq 2 ]
then
  cd data
  rm "data.json"
  touch "data.json"
  cd ../
  cd GIT-IO
  stack build
  printf "\033[0;36mGetting all commits for: \033[0;32mgithub.com/$2\033[0m...\n"
  stack exec -- GIT-IO-exe -i $1 $2
  cd ../
fi
printf "\033[0;34mServer Started at http://localhost:8000 \nctrl c to quit\033[0m\n"
python -m SimpleHTTPServer 8000
