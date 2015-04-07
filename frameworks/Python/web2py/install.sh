#!/bin/bash

mkdir -p $IROOT/.pip_cache

export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2

export PY2_ROOT=$IROOT/py2
export PY2_PIP=$PY2_ROOT/bin/pip

$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

cd $TROOT
rm -fr web2py
git clone --recursive --branch R-2.10.3 https://github.com/web2py/web2py.git 
cp -r app/app/ web2py/applications/
cp app/routes.py web2py/
