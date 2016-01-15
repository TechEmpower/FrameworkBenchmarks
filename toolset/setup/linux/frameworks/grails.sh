#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/grails.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/grails.installed
  return 0; }

VERSION="2.4.4"
GRAILS_HOME=$IROOT/grails-$VERSION

fw_get -O http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-$VERSION.zip
fw_unzip grails-$VERSION.zip

echo "export GRAILS_HOME=${GRAILS_HOME}" > $IROOT/grails.installed
echo -e "export PATH=\$GRAILS_HOME/bin:\$PATH" >> $IROOT/grails.installed
echo "export GRAILS_AGENT_CACHE_DIR=${IROOT}/.grails/.slcache" >> $IROOT/grails.installed

source $IROOT/grails.installed
