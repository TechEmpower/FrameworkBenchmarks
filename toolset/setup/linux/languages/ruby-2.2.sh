#!/bin/bash

fw_depends llvm-dev rvm

RETCODE=$(fw_exists ${IROOT}/ruby-2.2.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/ruby-2.2.installed
  return 0; }

# rvm stable [typically] only provides one version of ruby-2.2
# update this when it changes
MRI_VERSION=2.2.2

rvm install $MRI_VERSION
# Bundler is SOMETIMES missing... not sure why.
rvm $MRI_VERSION do gem install bundler

echo "export MRI_VERSION=${MRI_VERSION}" > $IROOT/ruby-2.2.installed

source $IROOT/ruby-2.2.installed
