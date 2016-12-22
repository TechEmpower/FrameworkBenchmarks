#!/bin/bash

fw_depends rvm java

RETCODE=$(fw_exists ${IROOT}/jruby-${JRUBY_VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/jruby-$JRUBY_VERSION.installed
  return 0; }

# rvm stable [typically] only provides one version of jruby-1.7
# update this when it changes
JRUBY_VERSION="1.7.9"

rvm install jruby-$JRUBY_VERSION
# Bundler is SOMETIMES missing... not sure why.
rvm jruby-$JRUBY_VERSION do gem install bundler

echo "export JRUBY_VERSION=${JRUBY_VERSION}" > $IROOT/jruby-$JRUBY_VERSION.installed

source $IROOT/jruby-$JRUBY_VERSION.installed
