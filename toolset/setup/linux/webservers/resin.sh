#!/bin/bash

RVER=4.0.41
RESIN=resin-$RVER
RESIN_HOME=$IROOT/$RESIN
RETCODE=$(fw_exists ${RESIN_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $RESIN_HOME.installed
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

echo "export RESIN_HOME=${RESIN_HOME}" > $RESIN_HOME.installed
echo -e "export PATH=${RESIN_HOME}/bin:\$PATH" >> $RESIN_HOME.installed

source $RESIN_HOME.installed
