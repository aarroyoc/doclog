#!/usr/bin/env bash

DOCLOG=$(realpath $(dirname $0))
SOURCE=$(realpath $1)
OUTPUT=$(realpath $2)

cd $DOCLOG
scryer-prolog -g 'run("'$SOURCE'", "'$OUTPUT'").' -g 'halt' main.pl
cd -
