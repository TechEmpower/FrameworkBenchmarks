#!/bin/bash

fw_depends python3

sed -i 's|host: 127.0.0.1|host: '${DBHOST}'|g' aiohttp.web/etc/hello/main/main.yaml

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

cd $TROOT/aiohttp.web
api_hour -ac hello:Container &
