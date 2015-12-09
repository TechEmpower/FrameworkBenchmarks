#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/java7.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/java7.installed
  return 0; }

JAVA=$IROOT/java7

# TODO: Someday move away from apt-get
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
# First remove java6
sudo apt-get remove -y --purge openjdk-6-jre openjdk-6-jre-headless
# Then install java7
echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo apt-get install -y oracle-java7-installer

# Setup environment variables
echo "export JAVA_HOME=/usr/lib/jvm/java-7-oracle" > $IROOT/java7.installed
echo -e "export PATH=\$JAVA_HOME/bin:\$PATH" >> $IROOT/java7.installed

source $IROOT/java7.installed
