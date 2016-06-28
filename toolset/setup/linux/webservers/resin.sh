#!/bin/bash

fw_depends java

RETCODE=$(fw_exists ${IROOT}/resin.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/resin.installed
  return 0; }

RVER=4.0.41
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
cat $FWROOT/config/resin.properties > conf/resin.properties

mv conf/resin.xml conf/resin.xml.orig
cat $FWROOT/config/resin.xml > conf/resin.xml

echo "export RESIN_HOME=${RESIN_HOME}" > $IROOT/resin.installed
echo -e "export PATH=\$RESIN_HOME/bin:\$PATH" >> $IROOT/resin.installed

source $IROOT/resin.installed
