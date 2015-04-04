#!/bin/bash

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 python3 

export PY2_ROOT=$IROOT/py2
export PY2_PIP=$PY2_ROOT/bin/pip

export PY3_ROOT=$IROOT/py3
export PY3_PIP=$PY3_ROOT/bin/pip3

$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

$PY3_PIP install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

