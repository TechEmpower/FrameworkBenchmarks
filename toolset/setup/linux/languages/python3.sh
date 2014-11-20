#!/bin/bash

RETCODE=$(fw_exists py3)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/3.4.2/Python-3.4.2.tar.xz
fw_untar Python-3.4.2.tar.xz
pre=$(pwd)
cd Python-3.4.2
./configure --prefix=${pre}/py3 --disable-shared --quiet
make -j4 --quiet
make install --quiet
cd ..
