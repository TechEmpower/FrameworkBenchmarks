#!/bin/bash

RVER=4.0.41

RETCODE=$(fw_exists resin-$RVER/conf/resin.xml)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends java
sudo cp -r /usr/lib/jvm/java-1.7.0-openjdk-amd64/include /usr/lib/jvm/java-1.7.0-openjdk-amd64/jre/bin/

fw_get http://www.caucho.com/download/resin-$RVER.tar.gz
fw_untar resin-$RVER.tar.gz
cd resin-$RVER
./configure --prefix=`pwd`
make
make install

mv conf/resin.properties conf/resin.properties.orig
cat $FWROOT/config/resin.properties > conf/resin.properties

mv conf/resin.xml conf/resin.xml.orig
cat $FWROOT/config/resin.xml > conf/resin.xml
