#!/usr/bin/env sh

if [ -z "$1" ]; then
	echo "Usage: <mk.sh> <url> <target folder>"
	exit 1
fi

if [ -z "$2" ]; then
	echo "Usage: <mk.sh> <url> <target folder>"
	exit 1
fi

mkdir -p "$2"
cd "$2" || exit
aria2c "$1" -o index.m3u8 --auto-file-renaming=false
python $(dirname $0)/parse_m3u8.py "$1" -o ./dl.list
aria2c -i ./dl.list -j10
ffmpeg -i index.m3u8 -vcodec copy "$2".mp4
mv "$2.mp4" ..
cd .. && rm -r "$2"
