#!/bin/bash

fw_depends rvm java

RETCODE=$(fw_exists ${IROOT}/jruby-9k.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/jruby-9k.installed
  return 0; }

# rvm stable [typically] only provides one version of jruby-9.0
# update this when it changes
JRUBY_VERSION="9.0.0.0.pre1"

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

echo "export JRUBY_VERSION=${JRUBY_VERSION}" > $IROOT/jruby-9k.installed

source $IROOT/jruby-9k.installed
