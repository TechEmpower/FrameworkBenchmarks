#!/bin/bash

RETCODE=$(fw_exists py3)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/3.4.1/Python-3.4.1.tar.xz
tar vxf Python-3.4.1.tar.xz
pre=$(pwd)
cd Python-3.4.1
./configure --prefix=${pre}/py3 --disable-shared
make -j4
make install

cd ..
py3/bin/pip3 install -r $FWROOT/config/requirements-py3.txt
