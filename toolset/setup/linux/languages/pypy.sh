#!/bin/bash

fw_exists pypy
[ $? -ne 0 ] || { return 0; }

fw_get https://bitbucket.org/pypy/pypy/downloads/pypy-2.3.1-linux64.tar.bz2 -O pypy-2.3.1-linux64.tar.bz2
tar vxf pypy-2.3.1-linux64.tar.bz2
ln -sf pypy-2.3.1-linux64 pypy

fw_get https://bootstrap.pypa.io/get-pip.py
pypy/bin/pypy get-pip.py

pypy/bin/pip install -r ../config/requirements-pypy.txt
