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

$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

$PY3_PIP install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt
