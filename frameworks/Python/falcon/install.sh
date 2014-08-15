#!/bin/bash

mkdir -p ~/.pip_cache
export PIP_DOWNLOAD_CACHE=$HOME/.pip_cache

fw_depends python2 python3 pypy

$IROOT/py2/bin/pip install -r $TROOT/requirements.txt
$IROOT/py3/bin/pip3 install -r $TROOT/requirements.txt
$IROOT/pypy/bin/pip install -r $TROOT/requirements-pypy.txt
