#!/bin/bash

set -x

RETCODE=$(fw_exists mono.installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Installing RootCAs from Mozilla..."; 
  mozroots --import --sync;
  echo "ROOT: Installing RootCAs from Mozilla..."; 
  sudo $IROOT/mono-3.6.0-install/bin/mozroots --import --sync;
  return 0; }

sudo apt-get install -qqy build-essential \
             autoconf \
             automake \
             libtool \
             zlib1g-dev \
             pkg-config \
             gettext

fw_get http://download.mono-project.com/sources/mono/mono-3.6.0.tar.bz2 -O mono-3.6.0.tar.bz2
fw_untar mono-3.6.0.tar.bz2

cd mono-3.6.0
./autogen.sh --prefix=$IROOT/mono-3.6.0-install
# make -j4 EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/basic.exe
echo -n "Installing Mono"
make -j4 2>&1 | tee $IROOT/mono-install.log | awk '{ if (NR%100 == 0) printf "."}'
make install 2>&1 | tee -a $IROOT/mono-install.log | awk '{ if (NR%100 == 0) printf "."}'

echo "Installing RootCAs from Mozilla..."; 
mozroots --import --sync;

echo "ROOT: Installing RootCAs from Mozilla..."; 
sudo $IROOT/mono-3.6.0-install/bin/mozroots --import --sync;

touch $IROOT/mono.installed
