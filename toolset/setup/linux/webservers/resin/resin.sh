#!/bin/bash

fw_depends java

fw_installed resin && return 0

RVER=4.0.48
RESIN=resin-$RVER
RESIN_HOME=$IROOT/$RESIN

sudo cp -r $JAVA_HOME/include $JAVA_HOME/jre/bin/

fw_get -O http://www.caucho.com/download/resin-$RVER.tar.gz
fw_untar resin-$RVER.tar.gz
cd resin-$RVER
./configure --prefix=`pwd`
make
make install

mv conf/resin.properties conf/resin.properties.orig
cat $FWROOT/toolset/setup/linux/webservers/resin/resin.properties > conf/resin.properties

mv conf/resin.xml conf/resin.xml.orig
cat $FWROOT/toolset/setup/linux/webservers/resin/resin.xml > conf/resin.xml

echo "export RESIN_HOME=${RESIN_HOME}" > $IROOT/resin.installed
echo -e "export PATH=\$RESIN_HOME/bin:\$PATH" >> $IROOT/resin.installed

source $IROOT/resin.installed
