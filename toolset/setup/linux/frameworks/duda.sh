#!/bin/bash

RETCODE=$(fw_exists duda-0.23.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://duda.io/releases/duda-client/dudac-0.23.tar.gz -O
fw_untar dudac-0.23.tar.gz

cd dudac-0.23

./dudac -r
./dudac -s

cd ..
touch duda-0.23.installed
