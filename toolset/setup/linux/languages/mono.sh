. ../toolset/setup/linux/bash_functions.sh

fw_exists mono-3.2.8
[ $? -ne 0 ] || { \
  echo "Mono is installed, installing RootCAs from Mozilla..."; 
  mozroots --import --sync;
  echo "Mono is installed!"
  return 0; }

fw_get http://download.mono-project.com/sources/mono/mono-3.2.8.tar.bz2 -O mono-3.2.8.tar.bz2
tar vxf mono-3.2.8.tar.bz2
cd mono-3.2.8 
./configure --disable-nls --prefix=/usr/local
make get-monolite-latest
make -j4 EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/basic.exe
sudo make install

mv mono-3.2.8 mono

mozroots --import --sync;
