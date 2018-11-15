#!/bin/sh
cd GIT-IO
stack build && stack exec -- GIT-IO-exe -i $1 $2
cd ../
echo "execution done"
