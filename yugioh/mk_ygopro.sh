#!/usr/bin/env bash

# compose a playable ygopro from scratch (mycard)

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

# building ygopro itself
git clone https://github.com/mycard/ygopro --recursive
cd ygopro
mkdir build && cd build
cmake -DLUA_INCLUDE_DIR=../../lua-5.3.5/src \
	-DLUA_LIBRARY=../../lua-5.3.5/src/liblua5.3-c++.a \
	-DCMAKE_CXX_FLAGS="-fno-rtti" \
	-DCMAKE_BUILD_TYPE=Release \
	-DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
	-G "Unix Makefiles" ..
make -j8

cd ../..
