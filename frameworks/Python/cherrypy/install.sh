#!/bin/bash

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 
#python3 pypy

wget https://pypi.python.org/packages/source/C/CherryPy/CherryPy-3.6.0.tar.gz -O cherrypy.tar.gz
tar -zxvf cherrypy.tar.gz
cd CherryPy-3.6.0
sudo python setup.py install


#$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

#$PY3_PIP install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

#$PYPY_PIP install --install-option="--prefix=${PYPY_ROOT}" -r $TROOT/requirements-pypy.txt
