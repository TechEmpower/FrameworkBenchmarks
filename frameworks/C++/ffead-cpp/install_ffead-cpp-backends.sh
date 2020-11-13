#!/bin/bash

cd $IROOT

git clone https://github.com/sumeetchhetri/ffead-cpp
cd ffead-cpp
git checkout e6fc4e54a266ee0af1cca7a5e0e6359c06129af7 -b 5.2
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
	mkdir build
	cd build
	cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_CTL=off -DBUILD_EXAMPLES=off -DBUILD_ORM=off ..
	make -j4 install
	cd $IROOT
	rm -rf drogon
fi

CURR_TYPE="nghttp2"
if [ "$CURR_TYPE" = "nghttp2" ]
then
	apt install --no-install-recommends -y libjansson-dev libc-ares-dev libboost-all-dev
	cd $IROOT
	wget -q https://github.com/nghttp2/nghttp2/releases/download/v1.41.0/nghttp2-1.41.0.tar.gz
	tar xf nghttp2-1.41.0.tar.gz
	cd nghttp2-1.41.0
	cmake -DENABLE_ASIO_LIB=on -GNinja .
	ninja install
	cd $IROOT
	rm -rf nghttp2-1.41.0 nghttp2-1.41.0.tar.gz
fi

CURR_TYPE="mongols"
if [ "$CURR_TYPE" = "mongols" ]
then
	cd $IROOT
	wget -q https://github.com/webcpp/mongols/archive/release-1.8.4.9.tar.gz
	tar xf release-1.8.4.9.tar.gz
	cd mongols-release-1.8.4.9/
	make clean && make -j4 && make install && ldconfig
	cp -rf inc/mongols/lib/* /usr/local/include/
	cd $IROOT
	rm -rf mongols-release-1.8.4.9/ release-1.8.4.9.tar.gz
fi

CURR_TYPE="uv-cpp"
if [ "$CURR_TYPE" = "uv-cpp" ]
then
	apt install --no-install-recommends -y libuv1-dev
	cd $IROOT
	git clone https://github.com/sumeetchhetri/uv-cpp
	cd uv-cpp
	cmake .
	make -j4 install
	cd $IROOT
	rm -rf uv-cpp
fi

CURR_TYPE="CppServer"
if [ "$CURR_TYPE" = "CppServer" ]
then
	apt install --no-install-recommends -y python3 python3-pip
	pip3 install gil
	cd $IROOT
	git clone https://github.com/chronoxor/CppServer
	cd CppServer
	gil update
	cd build
	./unix.sh
	cp $IROOT/CppServer/bin/libcppserver.a /usr/local/lib/
	cp -rf $IROOT/CppServer/modules/asio/asio/include/* /usr/local/include/
	cp -rf $IROOT/CppServer/modules/CppCommon/include/* /usr/local/include/
	cp -rf $IROOT/CppServer/include/* /usr/local/include/
	cp $IROOT/CppServer/temp/modules/libasio.a /usr/local/lib/
	cp $IROOT/CppServer/temp/modules/CppCommon/libcppcommon.a /usr/local/lib/
	cp $IROOT/CppServer/temp/modules/CppCommon/modules/libfmt.a /usr/local/lib/
	cd $IROOT
	rm -rf CppServer
fi

rm -rf /var/lib/apt/lists/*
