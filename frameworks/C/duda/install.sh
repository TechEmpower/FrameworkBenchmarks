#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/duda-0.23.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone https://github.com/monkey/dudac.git

cd dudac/

./dudac -r
./dudac -s

cd ..
touch ${IROOT}/duda-0.23.installed