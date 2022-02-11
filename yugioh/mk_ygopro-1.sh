#!/usr/bin/env bash
# build basic stuffs

if [ -z ${DEST} ]; then
	echo DEST is empty
	exit 1
fi

mkdir "${DEST}"
cd "${DEST}" || exit 1

# building lua
# https://github.com/Fluorohydride/ygopro/issues/2298
wget -O - https://www.lua.org/ftp/lua-5.3.5.tar.gz | tar zfx -
cd lua-5.3.5
sed -i '9c CC= g++' src/Makefile
sed -i '22c MYCFLAGS=-x c++' src/Makefile
sed -i '31c LUA_A=	liblua5.3-c++.a' src/Makefile
sed -i '44c TO_LIB= liblua5.3-c++.a' Makefile
make linux -j8

cd ..

# pulling database
git clone https://github.com/mycard/ygopro-database --depth=1

# getting irrKlang for audio
curl -OL https://www.ambiera.at/downloads/irrKlang-64bit-1.6.0.zip
unzip x irrKlang-64bit-1.6.0.zip
rm irrKlang-64bit-1.6.0.zip

# building ygopro itself
git clone git@github.com:Frefreak/ygopro.git --recursive
rm -rf ygopro/irrKlang
cp -r irrKlang-64bit-1.6.0 ygopro/irrKlang
cd ygopro

export IRRKLANG_DIR=$(pwd)/irrKlang

mkdir build && cd build
cmake -DLUA_INCLUDE_DIR=../../lua-5.3.5/src \
	-DLUA_LIBRARY=../../lua-5.3.5/src/liblua5.3-c++.a \
	-DUSE_IRRKLANG=1 \
	-DCMAKE_EXE_LINKER_FLAGS="-L ${IRRKLANG_DIR}/bin/linux-gcc-64 -l:ikpMP3.so -Wl,--enable-new-dtags" \
	-DCMAKE_CXX_FLAGS="-fno-rtti -I${IRRKLANG_DIR}/include -I${IRRKLANG_DIR}/plugins/ikpMP3" \
	-DCMAKE_BUILD_TYPE=Release \
	-DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
	-G "Unix Makefiles" ..
make -j8

cd ../..

# pull sound files
git clone https://code.mycard.moe/mycard/ygopro-sounds --depth=1
