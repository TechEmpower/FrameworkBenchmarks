#!/bin/bash

JRUBY=$IROOT/jruby-1.7.8
RETCODE=$(fw_exists ${JRUBY}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $JRUBY.installed
  return 0; }

fw_depends rvm

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  rvmsudo rvm install jruby-1.7.8
else
  rvm install jruby-1.7.8
fi

# Bundler is SOMETIMES missing... not sure why.
rvm jruby-1.7.8 do gem install bundler

echo "" > $JRUBY.installed

source $JRUBY.installed
