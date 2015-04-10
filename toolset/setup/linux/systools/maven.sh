#!/bin/bash

INSTALLED=$IROOT/maven.installed
RETCODE=$(fw_exists ${INSTALLED})
[ ! "$RETCODE" == 0 ] || { \
  source $INSTALLED
  return 0; }

sudo apt-get -y install maven
mvn -version

touch $INSTALLED

source $INSTALLED
