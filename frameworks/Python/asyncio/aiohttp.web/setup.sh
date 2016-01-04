#!/bin/bash

fw_depends python3

sed -i 's|host: 127.0.0.1|host: '${DBHOST}'|g' aiohttp.web/etc/hello/main/main.yaml

$IROOT/py3/bin/pip install --install-option="--prefix=${IROOT}/py3" -r $TROOT/requirements.txt

cd $TROOT/aiohttp.web
api_hour -ac hello:Container &
