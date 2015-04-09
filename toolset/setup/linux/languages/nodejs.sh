#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nvm-0.24.1.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

# Install nvm (node version manager) v0.24.1
fw_get https://raw.githubusercontent.com/creationix/nvm/v0.24.1/install.sh | \ 
	NVM_DIR=${IROOT}/nvm bash

export NVM_HOME=${IROOT}/nvm
export PATH=$PATH:$NVM_HOME/nvm.sh

touch ${IROOT}/nvm-0.24.1.installed