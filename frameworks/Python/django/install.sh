#!/bin/bash

export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_PIP=$PY2_ROOT/bin/pip
export PY2_GUNICORN=$PY2_ROOT/bin/gunicorn

export PYPY_ROOT=$IROOT/pypy
export PYPY=$PYPY_ROOT/bin/python
export PYPY_PIP=$PYPY_ROOT/bin/pip
export PYPY_GUNICORN=$PYPY_ROOT/bin/gunicorn

export PY3_ROOT=$IROOT/py3
export PY3=$PY3_ROOT/bin/python
export PY3_PIP=$PY3_ROOT/bin/pip3
export PY3_GUNICORN=$PY3_ROOT/bin/gunicorn

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 python3

$IROOT/py2/bin/pip install --install-option="--prefix=${IROOT}/py2" -r $TROOT/requirements.txt

$IROOT/py3/bin/pip3 install --install-option="--prefix=${IROOT}/py3" -r $TROOT/requirements.txt
