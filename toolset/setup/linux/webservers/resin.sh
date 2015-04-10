#!/bin/bash

RVER=4.0.41
RESIN=resin-$RVER
RESIN_HOME=$IROOT/$RESIN
INSTALLED=$RESIN_HOME.installed
RETCODE=$(fw_exists ${INSTALLED})
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $INSTALLED
  return 0; }

fw_depends java7
sudo cp -r $JAVA_HOME/include $JAVA_HOME/jre/bin/

fw_get http://www.caucho.com/download/$RESIN.tar.gz
fw_untar $RESIN.tar.gz
cd $RESIN
./configure --prefix=`pwd`
make
make install

mv conf/resin.properties conf/resin.properties.orig
cat $FWROOT/config/resin.properties > conf/resin.properties

mv conf/resin.xml conf/resin.xml.orig
cat $FWROOT/config/resin.xml > conf/resin.xml

echo "export RESIN_HOME=${RESIN_HOME}" > $INSTALLED
echo "export PATH=${RESIN_HOME}/bin:$PATH" >> $INSTALLED
chmod +x $INSTALLED

source $INSTALLED
