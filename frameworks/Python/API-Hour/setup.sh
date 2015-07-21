#!/bin/bash

export PY3_ROOT=$IROOT/py3
export PY3=$PY3_ROOT/bin/python
export PY3_PIP=$PY3_ROOT/bin/pip3
export PY3_API_HOUR=$PY3_ROOT/bin/api_hour

$PY3_API_HOUR -ac --chdir=hello/ --config_dir=hello/etc/hello/ hello:Container &