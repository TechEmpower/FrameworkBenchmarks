#!/bin/bash

RBX=$IROOT/rbx-2.2.10
RETCODE=$(fw_exists ${RBX}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $RBX.installed
  return 0; }

fw_depends rvm

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  # Rubinus cannot find libc during configure unless
  # you specify bash as the shell.
  SHELL="/bin/bash" rvmsudo rvm install rbx-2.2.10
else
  SHELL="/bin/bash" rvm install rbx-2.2.10
fi

# Bundler is SOMETIMES missing... not sure why.
rvm rbx-2.2.10 do gem install bundler

echo "export LC_ALL=en_US.UTF-8" > $RBX.installed
echo "export LANG=en_US.UTF-8" >> $RBX.installed

source $RBX.installed
