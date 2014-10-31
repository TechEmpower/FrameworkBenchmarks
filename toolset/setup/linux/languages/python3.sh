#!/bin/bash

RETCODE=$(fw_exists py3)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/3.4.1/Python-3.4.1.tar.xz
fw_untar Python-3.4.1.tar.xz
pre=$(pwd)
cd Python-3.4.1
./configure --prefix=${pre}/py3 --disable-shared --quiet
make -j4 --quiet
make install --quiet
cd ..

if [ ! -f "get-pip.py" ]; then
fw_get https://bootstrap.pypa.io/get-pip.py -O get-pip.py
fi
./py3/bin/python get-pip.py
