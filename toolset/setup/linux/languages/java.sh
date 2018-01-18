#!/bin/bash

fw_installed java && return 0

# TODO: Someday get away from apt-get
sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install openjdk-8-jdk

# Setup environment variables
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
echo "export JAVA_HOME=${JAVA_HOME}" > $IROOT/java.installed
echo -e "export PATH=\$JAVA_HOME/bin:\$PATH" >> $IROOT/java.installed

source $IROOT/java.installed
