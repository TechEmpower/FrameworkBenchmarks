#!/bin/bash

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 python3 pypy nginx

$IROOT/py2/bin/virtualenv $IROOT/py2
$IROOT/py2/bin/pip install -r $TROOT/requirements.txt

$IROOT/py3/bin/python3 -m venv $IROOT/py3
$IROOT/py3/bin/pip3 install -r $TROOT/requirements.txt

$IROOT/pypy/bin/virtualenv $IROOT/pypy
$IROOT/pypy/bin/pip install -r $TROOT/requirements-pypy.txt
