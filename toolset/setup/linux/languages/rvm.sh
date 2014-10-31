#!/bin/bash

RETCODE=$(fw_exists rvm.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Assume single-user installation
  source $HOME/.rvm/scripts/rvm
  return 0;
}

# Never run installation inside of TRAVIS, 
# just rely on their default RVM installaton
[ "$TRAVIS" != "true" ] || { \
  source $HOME/.rvm/scripts/rvm
  touch rvm.installed
  return 0;
}

# Run single-user installation and source rvm
gpg --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3
\curl -sSL https://get.rvm.io | SHELL=/bin/bash bash -s stable
source $HOME/.rvm/scripts/rvm

touch $IROOT/rvm.installed
