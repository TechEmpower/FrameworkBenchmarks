#!/bin/bash

# TODO include a check before we do all this, because it's 
# annoyingly slow to run apt-get if we don't need to

RETCODE=$(fw_exists java7.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  . $IROOT/java7.installed
  return 0; }

# First remove java6
sudo apt-get remove -y --purge openjdk-6-jre openjdk-6-jre-headless
# Then install java7
sudo apt-get install -y openjdk-7-jdk

# Setup environment variables
echo "export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-`dpkg --print-architecture`" > $IROOT/java7.installed
echo "export PATH=$JAVA_HOME/bin:$PATH" >> $IROOT/java7.installed