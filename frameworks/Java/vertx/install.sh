#!/bin/bash

fw_depends java8

RETCODE=$(fw_exists ${IROOT}/vert.x-2.1.5.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://dl.bintray.com/vertx/downloads/vert.x-2.1.5.tar.gz?direct=true -o vert.x-2.1.5.tar.gz
fw_untar vert.x-2.1.5.tar.gz
fw_get http://central.maven.org/maven2/org/freemarker/freemarker/2.3.22/freemarker-2.3.22.jar -o $IROOT/vert.x-2.1.5/lib/freemarker-2.3.22.jar

touch $IROOT/vert.x-2.1.5.installed
