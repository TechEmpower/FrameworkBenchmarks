#!/bin/bash

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 python3 pypy nginx

$IROOT/py2/bin/pip install --install-option="--prefix=${IROOT}/py2" -r $TROOT/requirements.txt

$IROOT/py3/bin/pip3 install --install-option="--prefix=${IROOT}/py3" -r $TROOT/requirements.txt

$IROOT/pypy/bin/pip install --install-option="--prefix=${IROOT}/pypy" -r $TROOT/requirements-pypy.txt
