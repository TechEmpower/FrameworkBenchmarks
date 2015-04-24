#!/bin/bash

fw_depends java7

RETCODE=$(fw_exists ${IROOT}/vert.x-2.1.5.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://dl.bintray.com/vertx/downloads/vert.x-2.1.5.tar.gz?direct=true -O vert.x-2.1.5.tar.gz
fw_untar vert.x-2.1.5.tar.gz

touch ${IROOT}/vert.x-2.1.5.installed
