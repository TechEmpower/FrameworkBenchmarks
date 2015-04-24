#!/bin/bash

RETCODE=$(fw_exists rvm.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Assume single-user installation
  source $IROOT/rvm.installed
  return 0;
}

# Never run installation inside of TRAVIS, 
# just rely on their default RVM installaton
[ "$TRAVIS" != "true" ] || { \
  echo "source /home/travis/.rvm/scripts/rvm" > rvm.installed
  source $IROOT/rvm.installed
  return 0;
}

# Run single-user installation and source rvm
export SHELL=/bin/bash
gpg --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3
\curl -sSL https://get.rvm.io | bash -s stable

echo "source ~/.rvm/scripts/rvm" > $IROOT/rvm.installed

source $IROOT/rvm.installed
