#!/bin/bash

mkdir -p $IROOT/.pip_cache

export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2

export PY2_ROOT=$IROOT/py2
export PY2_PIP=$PY2_ROOT/bin/pip

$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt
