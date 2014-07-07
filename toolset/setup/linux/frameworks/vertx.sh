#!/bin/bash

RETCODE=$(fw_exists vert.x-2.1.1)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://dl.bintray.com/vertx/downloads/vert.x-2.1.1.tar.gz?direct=true -O vert.x-2.1.1.tar.gz
fw_untar vert.x-2.1.1.tar.gz
