#!/bin/bash

fw_depends rvm java

fw_installed jruby-1.7 && return 0

# rvm stable [typically] only provides one version of jruby-1.7
# update this when it changes
JRUBY_VERSION="1.7.27"

rvm install jruby-$JRUBY_VERSION

rvm jruby-$JRUBY_VERSION do gem install bundler -v 1.15.4

echo "export JRUBY_VERSION=${JRUBY_VERSION}" > $IROOT/jruby-1.7.installed

source $IROOT/jruby-1.7.installed
