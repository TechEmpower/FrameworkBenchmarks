#!/bin/bash

export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_PIP=$PY2_ROOT/bin/pip
export PY2_GUNICORN=$PY2_ROOT/bin/gunicorn

export NGINX_HOME=${IROOT}/nginx

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 nginx

$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt
