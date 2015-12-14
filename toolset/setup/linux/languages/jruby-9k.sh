#!/bin/bash

fw_depends rvm java8

# rvm stable [typically] only provides one version of jruby-9.0
# update this when it changes
JRUBY_VERSION="9.0.0.0.pre1"

RETCODE=$(fw_exists ${IROOT}/jruby-${JRUBY_VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/jruby-$JRUBY_VERSION.installed
  return 0; }

rvm install jruby-$JRUBY_VERSION
# Bundler is SOMETIMES missing... not sure why.
rvm jruby-$JRUBY_VERSION do gem install bundler

echo "" > $IROOT/jruby-$JRUBY_VERSION.installed

source $IROOT/jruby-$JRUBY_VERSION.installed
