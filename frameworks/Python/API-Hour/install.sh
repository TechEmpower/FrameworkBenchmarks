#!/bin/bash

export PY3_ROOT=$IROOT/py3
export PY3=$PY3_ROOT/bin/python
export PY3_PIP=$PY3_ROOT/bin/pip3
export PY3_API_HOUR=$PY3_ROOT/bin/api_hour

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python3

$IROOT/py3/bin/pip3 install --install-option="--prefix=${IROOT}/py3" -r $TROOT/requirements.txt