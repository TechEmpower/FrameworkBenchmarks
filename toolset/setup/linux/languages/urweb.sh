#!/bin/bash

fw_exists /usr/local/bin/urweb
[ $? -ne 0 ] || { return 0; }

hg clone http://hg.impredicative.com/urweb
cd urweb
./autogen.sh
./configure
make
sudo make install
