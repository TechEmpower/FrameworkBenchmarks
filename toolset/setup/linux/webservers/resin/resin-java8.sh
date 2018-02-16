#!/bin/bash

fw_depends java8

fw_installed resin-java8 && return 0

RESIN_VERSION=4.0.55
RESIN_HOME=$IROOT/resin-java8/resin-$RESIN_VERSION

mkdir resin-java8
cd resin-java8
fw_get -O http://www.caucho.com/download/resin-$RESIN_VERSION.tar.gz
fw_untar resin-$RESIN_VERSION.tar.gz
cd resin-$RESIN_VERSION
./configure --prefix=`pwd`
make
make install

mv conf/resin.properties conf/resin.properties.orig
cat $FWROOT/toolset/setup/linux/webservers/resin/resin.properties > conf/resin.properties

mv conf/resin.xml conf/resin.xml.orig
cat $FWROOT/toolset/setup/linux/webservers/resin/resin.xml > conf/resin.xml

echo "export RESIN_HOME=${RESIN_HOME}" > $IROOT/resin-java8.installed
echo -e "export PATH=\$RESIN_HOME/bin:\$PATH" >> $IROOT/resin-java8.installed

source $IROOT/resin-java8.installed
