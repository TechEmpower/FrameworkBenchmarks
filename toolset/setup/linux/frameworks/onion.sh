. ../toolset/setup/linux/bash_functions.sh

fw_exists onion
[ $? -ne 0 ] || { echo "onion is installed!"; return 0; }

git clone https://github.com/davidmoreno/onion.git


cd onion
mkdir build
cd build
cmake ..
make