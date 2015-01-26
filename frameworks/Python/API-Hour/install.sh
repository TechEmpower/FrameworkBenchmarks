#!/bin/bash

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python3

$IROOT/py3/bin/pip3 install --install-option="--prefix=${IROOT}/py3" -r $TROOT/requirements.txt