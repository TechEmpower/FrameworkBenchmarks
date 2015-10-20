#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/java7.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/java7.installed
  return 0; }

JAVA=$IROOT/java7

# TODO: Someday move away from apt-get
# First remove java6
sudo apt-get remove -y --purge openjdk-6-jre openjdk-6-jre-headless
# Then install java7
sudo apt-get install -y openjdk-7-jdk

# Setup environment variables
echo "export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-`dpkg --print-architecture`" > $IROOT/java7.installed
echo -e "export PATH=\$JAVA_HOME/bin:\$PATH" >> $IROOT/java7.installed

source $IROOT/java7.installed
