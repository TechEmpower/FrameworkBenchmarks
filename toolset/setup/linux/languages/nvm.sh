#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nvm.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/nvm.installed
  return 0; }

fw_get -O https://raw.githubusercontent.com/creationix/nvm/v0.24.1/install.sh
NVM_DIR=$IROOT/nvm bash install.sh

echo "export NVM_HOME=${IROOT}/nvm" >> $IROOT/nvm.installed
echo -e "export PATH=${IROOT}/nvm:\$PATH" >> $IROOT/nvm.installed
