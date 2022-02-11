#!/usr/bin/env bash
# compose files

GAME_DIR=game
FONT=adobe-source-sans-pro

if [ -z ${DEST} ]; then
	echo DEST is empty
	exit 1
fi
cd $DEST || exit 1

FONT_FILE=$(fc-match ${FONT} --format=%{file})
if [ -z $FONT_FILE ]; then
	echo can not find font "${FONT}"
	exit 1
fi

rm -rf ${GAME_DIR}
mkdir -p ${GAME_DIR}
cp -r ygopro/build/bin/ygopro ygopro/{lflist.conf,script,system.conf,textures} $GAME_DIR
cp -r ygopro-database/locales/en-US/{cards.cdb,strings.conf} $GAME_DIR
cp -r ygopro-sounds/sound $GAME_DIR
cp -r ygopro-starter-pack/{deck,single} $GAME_DIR
cd $GAME_DIR
ln -s ../pics pics
sed -i "s|^textfont =.*|textfont = $FONT_FILE 14|" system.conf
sed -i "s|^numfont =.*|numfont = $FONT_FILE|" system.conf
