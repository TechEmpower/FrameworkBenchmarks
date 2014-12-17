#!/bin/bash

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 nginx

$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt