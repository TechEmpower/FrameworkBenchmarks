#!/bin/bash

fw_depends rvm

VERSION=2.1.2
RETCODE=$(fw_exists ${IROOT}/ruby-${VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/ruby-$VERSION.installed
  return 0; }

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  rvmsudo rvm install $VERSION
  # Bundler is SOMETIMES missing... not sure why.
  rvmsudo rvm $VERSION do gem install bundler
else
  rvm install $VERSION
  # Bundler is SOMETIMES missing... not sure why.
  rvm $VERSION do gem install bundler
fi

echo "" > $IROOT/ruby-$VERSION.installed

source $IROOT/ruby-$VERSION.installed
