#!/bin/bash

fw_depends rvm

# rvm stable [typically] only provides one version of ruby-2.0
# update this when it changes
MRI_VERSION=2.0.0-p643

RETCODE=$(fw_exists ${IROOT}/ruby-${MRI_VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/ruby-$MRI_VERSION.installed
  return 0; }

# We assume single-user installation as
# done in our rvm.sh script and
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  rvmsudo rvm install $MRI_VERSION
  # Bundler is SOMETIMES missing... not sure why.
  rvmsudo rvm $MRI_VERSION do gem install bundler
else
  rvm install $MRI_VERSION
  # Bundler is SOMETIMES missing... not sure why.
  rvm $MRI_VERSION do gem install bundler
fi

echo "" > $IROOT/ruby-$MRI_VERSION.installed

source $IROOT/ruby-$MRI_VERSION.installed
