#!/bin/bash

RETCODE=$(fw_exists resin-4.0.36/conf/resin.xml)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends java
sudo cp -r /usr/lib/jvm/java-1.7.0-openjdk-amd64/include /usr/lib/jvm/java-1.7.0-openjdk-amd64/jre/bin/

fw_get http://www.caucho.com/download/resin-4.0.36.tar.gz
fw_untar resin-4.0.36.tar.gz
cd resin-4.0.36
./configure --prefix=`pwd`
make
make install

mv conf/resin.properties conf/resin.properties.orig
cat $FWROOT/config/resin.properties > conf/resin.properties

mv conf/resin.xml conf/resin.xml.orig
cat $FWROOT/config/resin.xml > conf/resin.xml
