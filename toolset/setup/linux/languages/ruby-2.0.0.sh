#!/bin/bash

RUBY=$IROOT/ruby-2.0.0
RETCODE=$(fw_exists ${RUBY}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $RUBY.installed
  return 0; }

fw_depends rvm

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  rvmsudo rvm install 2.0.0-p0
else
  rvm install 2.0.0-p0
fi

# Bundler is SOMETIMES missing... not sure why.
rvm 2.0.0-p0 do gem install bundler

echo "" > $RUBY.installed

source $RUBY.installed
