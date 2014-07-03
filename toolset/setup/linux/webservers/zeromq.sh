#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/local/lib/libzmq.a
[ $? -ne 0 ] || { echo "ZeroMQ is installed!"; return 0; }

fw_get http://download.zeromq.org/zeromq-4.0.3.tar.gz
fw_untar zeromq-4.0.3.tar.gz
cd zeromq-4.0.3
./configure
make
sudo make install