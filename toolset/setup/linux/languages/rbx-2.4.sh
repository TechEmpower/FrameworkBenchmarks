#!/bin/bash

fw_depends rvm

RETCODE=$(fw_exists ${IROOT}/rbx-2.4.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/rbx-2.4.installed
  return 0; }

# rvm stable [typically] only provides one version of rbx-2.4
# update this when it changes
RBX_VERSION=2.4.1

# We assume single-user installation as
# done in our rvm.sh script and
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  # Rubinus cannot find libc during configure unless
  # you specify bash as the shell.
  SHELL="/bin/bash" rvmsudo rvm install rbx-$RBX_VERSION
  # Bundler is SOMETIMES missing... not sure why.
  SHELL="/bin/bash" rvmsudo rvm rbx-$RBX_VERSION do gem install bundler
else
  SHELL="/bin/bash" rvm install rbx-$RBX_VERSION
  # Bundler is SOMETIMES missing... not sure why.
  SHELL="/bin/bash" rvm rbx-$RBX_VERSION do gem install bundler
fi

echo "export LC_ALL=en_US.UTF-8" > $IROOT/rbx-2.4.installed
echo "export LANG=en_US.UTF-8" >> $IROOT/rbx-2.4.installed
echo "export RBX_VERSION=${RBX_VERSION}" >> $IROOT/rbx-2.4.installed

source $IROOT/rbx-2.4.installed
