#!/bin/bash

fw_depends java gcc-6

fw_installed resingcc6 && return 0

RVER=4.0.55
RESIN=resingcc6-$RVER
RESIN_HOME=$IROOT/$RESIN

fw_get -O http://www.caucho.com/download/resin-$RVER.tar.gz
fw_untar resin-$RVER.tar.gz
# We need separate folder so not to clash with the normal resin folder
mv resin-$RVER resingcc6-$RVER
cd resingcc6-$RVER
./configure --prefix=`pwd`
make
make install

mv conf/resin.properties conf/resin.properties.orig
cat $FWROOT/toolset/setup/linux/webservers/resin/resin.properties > conf/resin.properties

mv conf/resin.xml conf/resin.xml.orig
cat $FWROOT/toolset/setup/linux/webservers/resin/resin.xml > conf/resin.xml

echo "export RESIN_HOME=${RESIN_HOME}" > $IROOT/resingcc6.installed
echo -e "export PATH=\$RESIN_HOME/bin:\$PATH" >> $IROOT/resingcc6.installed

source $IROOT/resingcc6.installed
