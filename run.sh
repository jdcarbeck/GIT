#!/bin/sh
stack build && stack exec -- GIT-IO-exe -i $1 $2
echo "execution done"
