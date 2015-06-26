#!/bin/bash

fw_depends python3

pip install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

cd $TROOT/aiohttp.web
api_hour -ac hello:Container &
