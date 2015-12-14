#!/bin/bash

fw_depends rvm

VERSION=2.0.0-p0
RETCODE=$(fw_exists ${IROOT}/ruby-${VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/ruby-$VERSION.installed
  return 0; }

rvm install $VERSION
# Bundler is SOMETIMES missing... not sure why.
rvm $VERSION do gem install bundler

echo "" > $IROOT/ruby-$VERSION.installed

source $IROOT/ruby-$VERSION.installed
