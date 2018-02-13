#!/bin/bash

fw_installed java8 && return 0

# TODO: Someday get away from apt-get
sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install -qqy openjdk-8-jdk

# https://bugs.launchpad.net/ubuntu/+source/ca-certificates-java/+bug/1396760
sudo /var/lib/dpkg/info/ca-certificates-java.postinst configure

# Setup environment variables
JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
echo "export JAVA_HOME=${JAVA_HOME}" > $IROOT/java8.installed
echo -e "export PATH=\$JAVA_HOME/bin:\$PATH" >> $IROOT/java8.installed

source $IROOT/java8.installed
