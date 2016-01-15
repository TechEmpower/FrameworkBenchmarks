#!/bin/bash

RETCODE=$(fw_exists java.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/java.installed
  return 0; }

# TODO: Someday get away from apt-get
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo apt-get install -y oracle-java8-installer

# Setup environment variables
JAVA_HOME=/usr/lib/jvm/java-8-oracle
echo "export JAVA_HOME=${JAVA_HOME}" > $IROOT/java.installed
echo -e "export PATH=\$JAVA_HOME/bin:\$PATH" >> $IROOT/java.installed

source $IROOT/java.installed
