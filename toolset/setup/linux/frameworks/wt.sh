#!/bin/bash

RETCODE=$(fw_exists wt.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

# The commented code works. While we would love to get boost from source
# so we know exactly what we are getting, it just takes too long. Also, 
# Ubuntu1204 can only run boost 1.48 and Ubuntu1404 can only run 1.54, 
# even if you compile from source. Apt supplies different boost version 
# numbers anyways (which is something it often does not do and one of the 
# main reasons for compilation from a specific source version), so we can 
# just use apt. See https://github.com/TechEmpower/FrameworkBenchmarks/issues/1013
#
#fw_get http://downloads.sourceforge.net/project/boost/boost/1.48.0/boost_1_48_0.tar.gz -O boost_1_48_0.tar.gz
#fw_untar boost_1_48_0.tar.gz
#cd boost_1_48_0
#./bootstrap.sh --prefix=$IROOT/boost
#./b2 install
#cd ..

# Instead of compiling from source, just use apt to install onto 
# host machine
if [ "$TFB_DISTRIB_CODENAME" == "trusty" ]; then
    sudo apt-get -y install libboost1.54-all-dev
elif [ "$TFB_DISTRIB_CODENAME" == "precise" ]; then
    sudo apt-get -y install libboost1.48-all-dev
fi

fw_get http://downloads.sourceforge.net/witty/wt-3.3.3.tar.gz -O wt-3.3.3.tar.gz
fw_untar wt-3.3.3.tar.gz

cd wt-3.3.3
mkdir -p build
cd build
cmake .. -DWT_CPP_11_MODE=-std=c++0x -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=${IROOT}/wt -DCONFIGDIR=${IROOT}/wt/etc \
  -DCMAKE_CXX_COMPILER=$(which g++-4.8) -DDESTDIR=${IROOT}/wt \
  -DWEBUSER=$(id -u -n) -DWEBGROUP=$(id -g -n)
make
make install

cd ..
touch wt.installed