#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nvm.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get https://raw.githubusercontent.com/creationix/nvm/v0.24.1/install.sh
NVM_DIR=$IROOT/nvm bash install.sh

touch ${IROOT}/nvm.installed