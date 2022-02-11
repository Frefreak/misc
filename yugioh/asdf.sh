#!/usr/bin/env bash

GAME_DIR=game
FONT=adobe-source-sans-pro

cd $DEST
if [ -z ${DEST} ]; then
	echo DEST is empty
	exit 1
fi

FONT_FILE=$(fc-match ${FONT} --format=%{file})
if [ -z $FONT_FILE ]; then
	echo can not find font "${FONT}"
	exit 1
fi

mkdir -p $GAME_DIR
cp -r ygopro/build/bin/ygopro ygopro/{lflist.conf,script,system.conf} $GAME_DIR
cp -r ygopro-database/locales/en-US/{cards.cdb,strings.conf} $GAME_DIR
cd $GAME_DIR
sed -i "s/^textfont =.*/textfont = $FONT_FILE 14/" system.conf
sed -i "s/^numfont =.*/numfont = $FONT_FILE/" system.conf
