#!/bin/bash

fw_depends mysql postgresql python3

/usr/bin/python3.6 -m venv $TROOT/venv/ --clear

$TROOT/venv/bin/pip install -r $TROOT/requirements.txt

cd $TROOT/yocto_http
$TROOT/venv/bin/api_hour -ac hello:Container &
