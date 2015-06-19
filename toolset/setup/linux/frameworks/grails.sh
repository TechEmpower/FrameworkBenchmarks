#!/bin/bash

VERSION="2.4.4"
GRAINS_HOME=$IROOT/grains-$VERSION
RETCODE=$(fw_exists ${IROOT}/grails-2.4.4.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $GRAILS_HOME.installed
  return 0; }

fw_get http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-$VERSION.zip -O
fw_unzip grails-$VERSION.zip

echo "export GRAILS_HOME=${GRAILS_HOME}" > $GRAILS_HOME.installed
echo -e "export PATH=${GRAILS_HOME}/bin:\$PATH" >> $GRAILS_HOME.installed
echo "export GRAILS_AGENT_CACHE_DIR=${IROOT}/.grails/.slcache" >> $GRAILS_HOME.installed

source $GRAILS_HOME.installed
