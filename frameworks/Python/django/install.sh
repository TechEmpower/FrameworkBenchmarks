#!/bin/bash

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 python3

$IROOT/py2/bin/virtualenv $IROOT/py2
$IROOT/py2/bin/pip install -r $TROOT/requirements.txt

$IROOT/py3/bin/python3 -m venv $TROOT/py3
$IROOT/py3/bin/pip3 install -r $TROOT/requirements.txt
