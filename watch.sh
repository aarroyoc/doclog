#!/usr/bin/env bash
## Use inotifywait (from inotify-tools) to rebuild everytime the SOURCE changed.
## It expects the same 2 arguments as doclog.sh (SOURCE and OUTPUT).

DOCLOG=$(realpath $(dirname $0))
SOURCE=$1
OUTPUT=$2

doclog_rebuild() {
	rm -rf $OUTPUT
	$DOCLOG/doclog.sh $SOURCE $OUTPUT
}

doclog_rebuild

while true; do
	inotifywait -e modify,create,delete --exclude $OUTPUT -r $SOURCE
	doclog_rebuild
done
