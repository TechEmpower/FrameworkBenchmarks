#!/bin/bash


RETCODE=$(fw_exists mono-3.2.8.installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Installing RootCAs from Mozilla..."; 
  mozroots --import --sync;
  return 0; }

fw_get http://download.mono-project.com/sources/mono/mono-3.2.8.tar.bz2 -O mono-3.2.8.tar.bz2
fw_untar mono-3.2.8.tar.bz2
cd mono-3.2.8 
./configure --disable-nls --prefix=`pwd`/mono-3.2.8-install
make get-monolite-latest
make -j4 EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/basic.exe
sudo make install

ls -laR .

sudo apt-get -y install mono-devel
echo "Installing RootCAs from Mozilla..."; 
mozroots --import --sync;

touch mono-3.2.8.installed