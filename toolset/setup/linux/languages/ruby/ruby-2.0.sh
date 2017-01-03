#!/bin/bash

fw_depends llvm-dev rvm

fw_installed ruby-2.0 && return 0

# rvm stable [typically] only provides one version of ruby-2.0
# update this when it changes
MRI_VERSION=2.0.0-p643

rvm install $MRI_VERSION
# Bundler is SOMETIMES missing... not sure why.
rvm $MRI_VERSION do gem install bundler

echo "export MRI_VERSION=${MRI_VERSION}" > $IROOT/ruby-2.0.installed

source $IROOT/ruby-2.0.installed
