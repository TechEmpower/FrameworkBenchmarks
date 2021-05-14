#!/bin/bash

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
	wget -q https://github.com/nghttp2/nghttp2/releases/download/v1.42.0/nghttp2-1.42.0.tar.gz
	tar xf nghttp2-1.42.0.tar.gz
	cd nghttp2-1.42.0
	cmake -DENABLE_ASIO_LIB=on -GNinja .
	ninja install
	cd $IROOT
	rm -rf nghttp2-1.42.0 nghttp2-1.42.0.tar.gz
fi

CURR_TYPE="mongols"
if [ "$CURR_TYPE" = "mongols" ]
then
	cd $IROOT
	wget -q https://github.com/webcpp/mongols/archive/release-1.8.4.12.tar.gz
	tar xf release-1.8.4.12.tar.gz
	cd mongols-release-1.8.4.12/
	make clean && make -j4 && make install && ldconfig
	cp -rf inc/mongols/lib/* /usr/local/include/
	cd $IROOT
	rm -rf mongols-release-1.8.4.12/ release-1.8.4.12.tar.gz
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
	cd $IROOT/CppServer/modules/CppCommon/modules/fmt && git checkout b9ab5c8836bbffbe0a877f64d6faef8fbf4fd394 -b works
	cd $IROOT/CppServer/build
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

CURR_TYPE="lsquic-no"
if [ "$CURR_TYPE" = "lsquic" ]
then
	apt install --no-install-recommends -y libunwind-dev golang libevent-dev
	cd $IROOT
	git clone https://boringssl.googlesource.com/boringssl
	cd boringssl
	git checkout b117a3a0b7bd11fe6ebd503ec6b45d6b910b41a1
	sed -i "s/-Werror//g" CMakeLists.txt
	cmake -DCMAKE_BUILD_TYPE=Release . && make -j4
	cd $IROOT 
	git clone https://github.com/litespeedtech/lsquic.git
	cd lsquic
	git submodule init
	git submodule update
	cmake -DBORINGSSL_DIR=${IROOT}/boringssl . && make install -j4
	cd $IROOT
	rm -rf lsquic boringssl
	apt remove -y golang
	apt autoremove -y
fi

rm -rf /var/lib/apt/lists/*
