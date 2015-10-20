#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/maven.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/maven.installed
  return 0; }

# TODO: Someday remove apt-get
sudo apt-get -y install maven
mvn -version

touch $IROOT/maven.installed

source $IROOT/maven.installed
