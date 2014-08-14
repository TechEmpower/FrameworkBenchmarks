#!/bin/bash

RETCODE=$(fw_exists wt)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://downloads.sourceforge.net/project/boost/boost/1.55.0/boost_1_55_0.tar.gz -O boost_1_55_0.tar.gz
fw_untar boost_1_55_0.tar.gz
cd boost_1_55_0
echo 'using gcc : 4.8 : g++-4.8 ;' > tools/build/v2/user-config.jam
./bootstrap.sh --prefix=${IROOT}/boost
./b2 --toolset gcc-4.8 install
cd ..

fw_get http://downloads.sourceforge.net/witty/wt-3.3.3.tar.gz -O wt-3.3.3.tar.gz
fw_untar wt-3.3.3.tar.gz

cd wt-3.3.3
mkdir -p build
cd build
cmake .. -DWT_CPP_11_MODE=-std=c++0x -DCMAKE_BUILD_TYPE=Release \
	-DBOOST_PREFIX=${IROOT}/boost -DCMAKE_INSTALL_PREFIX=${IROOT}/wt \
	-DCONFIGDIR=${IROOT}/wt/etc -DCMAKE_CXX_COMPILER=$(which g++-4.8)
make
make install
