#!/bin/sh
cd data
touch "$2_data.json"
cd ../
cd GIT-IO
stack build
printf "\033[1;36mGreenGetting all commits for: \033[1;32mgithub.com/$1/$2\033[0m...\n"
stack exec -- GIT-IO-exe -i $1 $2
printf "\033[0;34mDone\033[0m\n"
