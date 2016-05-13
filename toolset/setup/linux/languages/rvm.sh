#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/rvm.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Assume single-user installation
  source $IROOT/rvm.installed
  return 0;
}

export SHELL=/bin/bash

if [ "$TRAVIS" = "true" ]
then
  gpg --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3
  \curl -sSL https://get.rvm.io | sudo bash -s stable
  echo "source /usr/local/rvm/scripts/rvm" > $IROOT/rvm.installed
else
  gpg --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3
  \curl -sSL https://get.rvm.io | bash -s stable
  echo "source ~/.rvm/scripts/rvm" > $IROOT/rvm.installed
fi

source $IROOT/rvm.installed
