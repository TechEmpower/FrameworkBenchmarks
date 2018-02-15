#!/bin/bash

fw_installed java && return 0

fw_get -O https://download.java.net/java/GA/jdk9/9.0.4/binaries/openjdk-9.0.4_linux-x64_bin.tar.gz
fw_untar openjdk-9.0.4_linux-x64_bin.tar.gz

JAVA_HOME=$IROOT/jdk-9.0.4
echo "export JAVA_HOME=${JAVA_HOME}" > $IROOT/java.installed
echo -e "export PATH=\$JAVA_HOME/bin:\$PATH" >> $IROOT/java.installed

source $IROOT/java.installed
