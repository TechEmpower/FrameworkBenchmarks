#!/bin/bash

fw_depends llvm-dev libjemalloc-dev rvm

fw_installed ruby-2.3 && return 0

# rvm stable [typically] only provides one version of ruby-2.3
# update this when it changes
MRI_VERSION=2.3.5

rvm install $MRI_VERSION -C --with-jemalloc

rvm $MRI_VERSION do gem install bundler -v 1.15.4

echo "export MRI_VERSION=${MRI_VERSION}" > $IROOT/ruby-2.3.installed

source $IROOT/ruby-2.3.installed
