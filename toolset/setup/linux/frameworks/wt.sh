#!/bin/bash

fw_depends postgresql mysql gcc-6

fw_installed wt && return 0

WT_VERSION=4.0.2
BOOST_ROOT=${IROOT}/boost
BOOST_INC=${BOOST_ROOT}/include
BOOST_LIB=${BOOST_ROOT}/lib
WT_ROOT=${IROOT}/wt
WT_LIB=${WT_ROOT}/lib
WT_INC=${WT_ROOT}/include
LD_LIBRARY_PATH="${BOOST_LIB}:${WT_LIB}:${LD_LIBRARY_PATH}"
CPLUS_INCLUDE_PATH=/usr/include/postgresql:/usr/include/postgresql/9.3/server:$CPLUS_INCLUDE_PATH

# Install CMake 3.x
sudo apt-add-repository --yes ppa:george-edison55/cmake-3.x
sudo apt-get update -qq

sudo apt-get install -qqy \
  cmake

# Build boost_thread, boost_system, boost_filesystem and boost_program_options
fw_get -o boost_1_65_1.tar.gz https://dl.bintray.com/boostorg/release/1.65.1/source/boost_1_65_1.tar.gz
fw_untar boost_1_65_1.tar.gz
cd boost_1_65_1
./bootstrap.sh
./b2 \
  -d0 \
  toolset=gcc-6 \
  variant=release \
  link=static \
  cxxflags="-std=c++14 -march=native" \
  cflags="-march=native" \
  --prefix=${BOOST_ROOT} \
  --with-system \
  --with-thread \
  --with-program_options \
  --with-filesystem \
  install
cd ..

fw_get -O https://github.com/emweb/wt/archive/$WT_VERSION.tar.gz
mv $WT_VERSION.tar.gz wt-$WT_VERSION.tar.gz
fw_untar wt-$WT_VERSION.tar.gz

cd wt-$WT_VERSION
mkdir -p build
cd build
cmake .. -DCMAKE_CXX_STANDARD=14 -DCMAKE_BUILD_TYPE=Release \
  -DBOOST_PREFIX=${BOOST_ROOT} \
  -DCMAKE_INSTALL_PREFIX=${IROOT}/wt -DCONFIGDIR=${IROOT}/wt/etc \
  -DCMAKE_C_COMPILER=$(which gcc-6) \
  -DCMAKE_CXX_COMPILER=$(which g++-6) -DDESTDIR=${IROOT}/wt \
  -DWEBUSER=$(id -u -n) -DWEBGROUP=$(id -g -n) \
  -DENABLE_SSL=OFF -DHTTP_WITH_ZLIB=OFF \
  -DCMAKE_C_FLAGS_RELEASE="-O3 -march=native -DNDEBUG" \
  -DCMAKE_CXX_FLAGS_RELEASE="-O3 -march=native -DNDEBUG" \
  -DBUILD_TESTS=OFF -DENABLE_LIBWTTEST=OFF \
  -DSHARED_LIBS=OFF
make
make install

cd $IROOT

echo "export BOOST_ROOT=${BOOST_ROOT}" > $IROOT/wt.installed
echo "export BOOST_INC=${BOOST_INC}" >> $IROOT/wt.installed
echo "export BOOST_LIB=${BOOST_LIB}" >> $IROOT/wt.installed
echo "export WT_ROOT=${WT_ROOT}" >> $IROOT/wt.installed
echo "export WT_LIB=${WT_LIB}" >> $IROOT/wt.installed
echo "export WT_INC=${WT_INC}" >> $IROOT/wt.installed
echo -e "export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:\$LD_LIBRARY_PATH" >> $IROOT/wt.installed
echo -e "export CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH}:\$CPLUS_INCLUDE_PATH" >> $IROOT/wt.installed

source $IROOT/wt.installed
