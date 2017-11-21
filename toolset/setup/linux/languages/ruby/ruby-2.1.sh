#!/bin/bash

fw_depends llvm-dev rvm

fw_installed ruby-2.1 && return 0

# TODO: Ruby 2.1 is EOL. Remove as soon as possible.
MRI_VERSION=2.1.10

rvm install $MRI_VERSION

rvm $MRI_VERSION do gem install bundler -v 1.15.4

echo "export MRI_VERSION=${MRI_VERSION}" > $IROOT/ruby-2.1.installed

source $IROOT/ruby-2.1.installed
