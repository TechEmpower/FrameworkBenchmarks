#!/bin/bash

RETCODE=$(fw_exists py2)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/2.7.8/Python-2.7.8.tgz
fw_untar Python-2.7.8.tgz
pre=$(pwd)
cd Python-2.7.8
./configure --prefix=${pre}/py2 --disable-shared --quiet
echo -n "Installing Python 2"
make -j4 --quiet 2>&1 | awk '{ if (NR%100 == 0) printf "."}'
make install --quiet 2>&1 | awk '{ if (NR%100 == 0) printf "."}'
cd ..

if [ ! -f "get-pip.py" ]; then
fw_get https://bootstrap.pypa.io/get-pip.py -O get-pip.py
fi
./py2/bin/python get-pip.py
./py2/bin/pip install virtualenv
