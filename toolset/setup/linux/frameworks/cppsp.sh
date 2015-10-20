#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/cppsp.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/cppsp.installed
  return 0; }

VERSION=0.2.3
CPPSP_HOME=$IROOT/cppsp_$VERSION

fw_get -o cppsp_$VERSION.tar.xz http://downloads.sourceforge.net/project/cpollcppsp/CPPSP%200.2%20%28testing%29/cppsp_$VERSION.tar.xz
fw_untar cppsp_$VERSION.tar.xz

# Using cp+rm over mv intentionally, because apparently this download
# causes oddball issues when mv'ed around inside a folder mounted 
# inside of VirtualBox (may have something to do with case-sensitive 
# filesystems)
cp -R cppsp_rel$VERSION/ $CPPSP_HOME
rm -rf cppsp_rel$VERSION/

sed -i 's|CXX := .*|CXX := g++-4.8|g' $CPPSP_HOME/makefile
sed -i 's|-Wall|-w|g' $CPPSP_HOME/makefile

echo "export CPPSP_HOME=${CPPSP_HOME}" > $IROOT/cppsp.installed
echo -e "export CPLUS_INCLUDE_PATH=/usr/include/postgresql:/usr/include/postgresql/9.3/server:\$CPLUS_INCLUDE_PATH" >> $IROOT/cppsp.installed

source $IROOT/cppsp.installed
