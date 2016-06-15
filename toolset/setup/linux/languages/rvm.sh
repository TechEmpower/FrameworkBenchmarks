#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/rvm.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Assume single-user installation
  source $IROOT/rvm.installed
  return 0;
}

export SHELL=/bin/bash
gpg --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3
RVM_INSTALLER="curl -sSL https://get.rvm.io"

if [ "$TRAVIS" = "true" ]
then
  $RVM_INSTALLER | sudo bash -s stable
  echo "source /usr/local/rvm/scripts/rvm" > $IROOT/rvm.installed
else
  $RVM_INSTALLER | bash -s stable
  echo "source ~/.rvm/scripts/rvm" > $IROOT/rvm.installed
fi

source $IROOT/rvm.installed
