#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/node-v0.10.8.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://nodejs.org/dist/v0.10.8/node-v0.10.8-linux-x64.tar.gz
fw_untar node-v0.10.8-linux-x64.tar.gz

touch ${IROOT}/node-v0.10.8.installed