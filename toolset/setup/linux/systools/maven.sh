#!/bin/bash

MAVEN=$IROOT/maven
RETCODE=$(fw_exists ${MAVEN}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $MAVEN.installed
  return 0; }

sudo apt-get -y install maven
mvn -version

touch $MAVEN.installed

source $MAVEN.installed
