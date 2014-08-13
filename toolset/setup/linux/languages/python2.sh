#!/bin/bash

RETCODE=$(fw_exists py2)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/2.7.7/Python-2.7.7.tgz
fw_untar Python-2.7.7.tgz
pre=$(pwd)
cd Python-2.7.7
./configure --prefix=${pre}/py2 --disable-shared
make -j4
make install

cd ..
fw_get https://bootstrap.pypa.io/get-pip.py -O get-pip2.py
py2/bin/python get-pip2.py
py2/bin/pip install -r $FWROOT/config/requirements.txt
