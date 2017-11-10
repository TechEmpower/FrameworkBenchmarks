#!/bin/bash

fw_depends rvm java

fw_installed jruby-9.0 && return 0

# TODO: JRuby 9.0 is EOL. Remove as soon as possible.
JRUBY_VERSION="9.0.5.0"

rvm install jruby-$JRUBY_VERSION

rvm jruby-$JRUBY_VERSION do gem install bundler -v 1.15.4

echo "export JRUBY_VERSION=${JRUBY_VERSION}" > $IROOT/jruby-9.0.installed

source $IROOT/jruby-9.0.installed
