#!/bin/bash

fw_depends rvm

RETCODE=$(fw_exists ${IROOT}/rbx.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/rbx.installed
  return 0; }

VERSION=2.2.10

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  # Rubinus cannot find libc during configure unless
  # you specify bash as the shell.
  SHELL="/bin/bash" rvmsudo rvm install rbx-$VERSION
  # Bundler is SOMETIMES missing... not sure why.
  SHELL="/bin/bash" rvmsudo rvm rbx-$VERSION do gem install bundler
else
  SHELL="/bin/bash" rvm install rbx-$VERSION
  # Bundler is SOMETIMES missing... not sure why.
  SHELL="/bin/bash" rvm rbx-$VERSION do gem install bundler
fi

echo "export LC_ALL=en_US.UTF-8" > $IROOT/rbx.installed
echo "export LANG=en_US.UTF-8" >> $IROOT/rbx.installed

source $IROOT/rbx.installed
