#!/bin/bash

export PY3_ROOT=$IROOT/py3
export PY3=$PY3_ROOT/bin/python3
export PY3_PIP=$PY3_ROOT/bin/pip3

$PY3 app.py &
