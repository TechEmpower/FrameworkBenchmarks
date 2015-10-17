#!/bin/bash

fw_depends rvm

JRUBY_VERSION="1.7.13"
RETCODE=$(fw_exists ${IROOT}/jruby-${JRUBY_VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/jruby-$JRUBY_VERSION.installed
  return 0; }

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  rvmsudo rvm install jruby-$JRUBY_VERSION
  # Bundler is SOMETIMES missing... not sure why.
  rvmsudo rvm jruby-$JRUBY_VERSION do gem install bundler
else
  rvm install jruby-$JRUBY_VERSION
  # Bundler is SOMETIMES missing... not sure why.
  rvm jruby-$JRUBY_VERSION do gem install bundler
fi

echo "" > $IROOT/jruby-$JRUBY_VERSION.installed

source $IROOT/jruby-$JRUBY_VERSION.installed
