#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/maven.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get -y install maven
mvn -version

touch ${IROOT}/maven.installed