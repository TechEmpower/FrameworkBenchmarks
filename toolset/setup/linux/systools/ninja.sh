#!/bin/bash

NINJA_VERSION="1.7.2"

fw_installed ninja && return 0

fw_get -O https://github.com/ninja-build/ninja/releases/download/v${NINJA_VERSION}/ninja-linux.zip
mkdir ${IROOT}/ninja
unzip ninja-linux.zip -d ${IROOT}/ninja/bin

echo -e "export PATH=${IROOT}/ninja/bin:\$PATH" > $IROOT/ninja.installed

source ${IROOT}/ninja.installed
