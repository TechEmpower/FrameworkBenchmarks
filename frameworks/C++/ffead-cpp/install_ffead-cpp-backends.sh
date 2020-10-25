#!/bin/bash

cd $IROOT

git clone https://github.com/sumeetchhetri/ffead-cpp
cd ffead-cpp
git checkout aad0799955d93793e0b3659f29deaa19f74c25fe -b 5.1
rm -rf .git
cd ..
mv ffead-cpp ffead-cpp-src
mv ffead-cpp-src/lang-server-backends ${IROOT}/
cd $IROOT

CURR_TYPE="lithium"
if [ "$CURR_TYPE" = "lithium" ]
then
	SRV_TYPE=SRV_LITHIUM
	apt install --no-install-recommends -y libboost-all-dev
fi

CURR_TYPE="cinatra"
if [ "$CURR_TYPE" = "cinatra" ]
then
	cd $IROOT
	apt install --no-install-recommends -y libboost-all-dev
	SRV_TYPE=SRV_CINATRA
	CINATRA_INC="-DCINATRA_INCLUDES=${IROOT}/cinatra/include"
	git clone https://github.com/sumeetchhetri/cinatra.git
	cd cinatra
	git checkout b3871a074f6107f57acf42a15fa872d4076436ab -b works
	rm -rf .git
fi

CURR_TYPE="drogon"
if [ "$CURR_TYPE" = "drogon" ]
then
	cd $IROOT
	apt install --no-install-recommends -y libjsoncpp-dev uuid-dev
	apt remove -y libsqlite3-dev
	SRV_TYPE=SRV_DROGON
	git clone --recurse-submodules https://github.com/sumeetchhetri/drogon
	cd  drogon
	git checkout a10934f3f85f361cde58a891d3cf1f1df3a8ea8a -b works
	rm -rf .git
	mkdir build
	cd build
	cmake -DCMAKE_BUILD_TYPE=Release ..
	make && make install
	cd $IROOT
	rm -rf drogon
fi

CURR_TYPE="nghttp2"
if [ "$CURR_TYPE" = "nghttp2" ]
then
	apt install --no-install-recommends -y libjansson-dev libc-ares-dev libboost-all-dev
	cd $IROOT
	wget -q https://github.com/nghttp2/nghttp2/releases/download/v1.41.0/nghttp2-1.41.0.tar.gz
	tar xvf nghttp2-1.41.0.tar.gz
	cd nghttp2-1.41.0
	cmake -DENABLE_ASIO_LIB=on -GNinja .
	ninja install
	cd $IROOT
	rm -rf nghttp2-1.41.0
fi

rm -rf /var/lib/apt/lists/*
