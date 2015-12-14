#!/bin/bash

fw_depends rvm

# rvm stable [typically] only provides one version of ruby-2.2
# update this when it changes
MRI_VERSION=2.2.1

RETCODE=$(fw_exists ${IROOT}/ruby-${MRI_VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/ruby-$MRI_VERSION.installed
  return 0; }

rvm install $MRI_VERSION
# Bundler is SOMETIMES missing... not sure why.
rvm $MRI_VERSION do gem install bundler

echo "" > $IROOT/ruby-$MRI_VERSION.installed

source $IROOT/ruby-$MRI_VERSION.installed
