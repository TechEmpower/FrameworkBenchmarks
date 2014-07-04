#!/bin/bash

fw_exists py2
[ $? -ne 0 ] || { return 0; }

# fw_get http://www.python.org/ftp/python/2.7.7/Python-2.7.7.tgz
# tar vxf Python-2.7.7.tgz
pre=$(pwd)
cd Python-2.7.7
./configure --prefix=${pre}/py2 --disable-shared
make -j4
make install

cd ..
fw_get https://bootstrap.pypa.io/get-pip.py -O get-pip2.py
py2/bin/python get-pip2.py
py2/bin/pip install -r ../config/requirements.txt
