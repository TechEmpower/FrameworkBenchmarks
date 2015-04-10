#!/bin/bash

JAVA=$IROOT/java7
INSTALLED=$JAVA.installed
RETCODE=$(fw_exists ${INSTALLED})
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $INSTALLED
  return 0; }

# First remove java6
sudo apt-get remove -y --purge openjdk-6-jre openjdk-6-jre-headless
# Then install java7
sudo apt-get install -y openjdk-7-jdk

# Setup environment variables
JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-`dpkg --print-architecture`
echo "export JAVA_HOME=${JAVA_HOME}" > $INSTALLED
echo "export PATH=${JAVA_HOME}/bin:$PATH" >> $INSTALLED
chmod +x $INSTALLED

source $INSTALLED
