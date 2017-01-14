#!/bin/bash

fw_depends rvm java

fw_installed jruby-9.1 && return 0

# rvm stable [typically] only provides one version of jruby-9.0
# update this when it changes
JRUBY_VERSION="9.1.7.0"

rvm install jruby-$JRUBY_VERSION
# Use a specific version of Bundler to work around thread concurrency issues.
rvm jruby-$JRUBY_VERSION do gem install bundler -v 1.13.7

echo "export JRUBY_VERSION=${JRUBY_VERSION}" > $IROOT/jruby-9.1.installed

source $IROOT/jruby-9.1.installed
