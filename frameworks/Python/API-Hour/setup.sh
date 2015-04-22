#!/bin/bash

fw_depends python3

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

api_hour -ac --chdir=hello/ --config_dir=hello/etc/hello/ hello:Container &
