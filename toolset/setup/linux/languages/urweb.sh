#!/bin/bash

RETCODE=$(fw_exists /usr/local/bin/urweb)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.impredicative.com/ur/urweb-20140819.tgz
fw_untar urweb-20140819.tgz
cd urweb-20140819
./configure
make
sudo make install
