. ../toolset/setup/linux/bash_functions.sh

fw_exists wt
[ $? -ne 0 ] || { echo "wt is installed!"; return 0; }

# TODO can this be changed based on the OS we are using? This is not
# available on 12.04 (libboost-all-dev is available, but requires some
# apt-get cleaning before it can be installed)
sudo apt-get install libboost1.54-all-dev

fw_get http://downloads.sourceforge.net/witty/wt-3.3.3.tar.gz -O wt.tar.gz
fw_untar wt.tar.gz

mv wt-* wt
cd wt
mkdir build
cd build
cmake .. -DWT_CPP_11_MODE=-std=c++0x -DCMAKE_BUILD_TYPE=Release
make
sudo make install

rm wt.tar.gz