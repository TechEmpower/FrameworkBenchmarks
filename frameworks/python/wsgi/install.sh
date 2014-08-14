#!/bin/bash

mkdir -p ~/.pip_cache
export PIP_DOWNLOAD_CACHE=$HOME/.pip_cache

fw_depends python2 nginx

$IROOT/py2/bin/pip install -r $TROOT/requirements.txt
