#!/usr/bin/env bash

DOCLOG=$(realpath $(dirname $0))

case "$(uname -s)" in
    MINGW*)
        SOURCE=$(cygpath -w $(realpath $1) | sed -e 's/\\/\\\\/g');
        OUTPUT=$(cygpath -w $(realpath $2) | sed -e 's/\\/\\\\/g');;
    *)
        SOURCE=$(realpath $1);
        OUTPUT=$(realpath $2);;
esac

cd $DOCLOG
scryer-prolog -g "run(\"$SOURCE\", \"$OUTPUT\")." -g 'halt' main.pl
cd -
