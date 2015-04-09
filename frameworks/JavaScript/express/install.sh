#!/bin/bash

fw_depends nodejs

export NVM_HOME=${IROOT}/nvm
source $NVM_HOME/nvm.sh
nvm install 0.10.8