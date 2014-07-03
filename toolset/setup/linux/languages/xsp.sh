#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists xsp
[ $? -ne 0 ] || { echo "XSP is installed!"; return 0; }

fw_depends mono
git clone --depth 1 git://github.com/mono/xsp
cd xsp
./autogen.sh --prefix=/usr/local
make
sudo make install
