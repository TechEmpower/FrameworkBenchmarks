#!/bin/bash

JAVA=$IROOT/java7
RETCODE=$(fw_exists ${JAVA}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $JAVA.installed
  return 0; }

# First remove java6
sudo apt-get remove -y --purge openjdk-6-jre openjdk-6-jre-headless
# Then install java7
sudo apt-get install -y openjdk-7-jdk

# Setup environment variables
JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-`dpkg --print-architecture`
echo "export JAVA_HOME=${JAVA_HOME}" > $JAVA.installed
echo -e "export PATH=${JAVA_HOME}/bin:\$PATH" >> $JAVA.installed

source $JAVA.installed
