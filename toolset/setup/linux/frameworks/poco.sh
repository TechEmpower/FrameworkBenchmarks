#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/poco.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/poco.installed
  return 0; }

VERSION=1.6.1
POCO_HOME=$IROOT/poco_$VERSION

fw_get -o poco_$VERSION.tar.gz http://pocoproject.org/releases/poco-$VERSION/poco-$VERSION-all.tar.gz
fw_untar poco_$VERSION.tar.gz

cp -R poco-$VERSION-all/ $POCO_HOME
rm -rf poco-$VERSION-all/

cd $POCO_HOME
./configure --no-tests --no-samples
make --quiet PageCompiler-libexec XML-libexec JSON-libexec

echo "export POCO_HOME=${POCO_HOME}" > $IROOT/poco.installed
echo "export LD_LIBRARY_PATH=$POCO_HOME/lib/Linux/x86_64" >> $IROOT/poco.installed 

source $IROOT/poco.installed

