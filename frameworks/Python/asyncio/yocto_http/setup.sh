#!/bin/bash

fw_depends mysql postgresql python3

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

cd $TROOT/yocto_http
api_hour -ac hello:Container &
