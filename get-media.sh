#!/bin/sh
# Purpose: download media files from HTTP server

URL=http://www.process-one.net/downloads/otms
MEDIADIR=applications/medias
FILES="big_buck_bunny.flv elephants_dream.flv"

for i in $FILES; do
    [ -f $MEDIADIR/$i ] || NEEDFILES="$NEEDFILES $i";
done

function download {
    for i in $NEEDFILES; do
	echo "Downloading $i to $MEDIADIR"
	case $CMD in
	    wget) wget $URL/$i -O $MEDIADIR/$i;;
	    curl) curl $URL/$i -o $MEDIADIR/$i;;
	esac;
    done
}

if [ -n "$NEEDFILES" ]; then
    if ( builtin type -p wget > /dev/null ); then
	CMD=wget;
	download;
    elif ( builtin type -p curl > /dev/null ); then
	CMD=curl;
	download;
    else
	echo "Unable to download media files: neither curl(1) nor wget(1) found."
	echo "You have several ways to fix it:"
	echo "1) install curl(1) or wget(1) and re-run the script"
	echo "2) download media files manually from $URL to $MEDIADIR directory"
	echo "3) if you have any FLV files, put them in $MEDIADIR directory"
	return 1
    fi;
fi
