#!/bin/bash

set -e

# Download's suil
RELEASE_NAME=v${SUIL_VERSION}
if [ -n "${SUIL_VERSION_TAG}" ] ; then
    RELEASE_NAME=${RELEASE_NAME}-${SUIL_VERSION_TAG}
fi

echo "Fetching suil {version=${SUIL_VERSION}} artifacts"
BASE_DIR=Suil-${SUIL_VERSION}-${SUIL_CONFIGURATION}
BINARY=${BASE_DIR}.tar.gz
DOWNLOAD_URL="https://github.com/dccarter/suil/releases/download/${RELEASE_NAME}/${BINARY}"
echo "Downloading suil from ${DOWNLOAD_URL}"
wget -q ${DOWNLOAD_URL} -O /tmp/${BINARY}
tar -xzf /tmp/${BINARY} -C /var
ls /var/${BASE_DIR}
echo "Suil upacked /var/${BASE_DIR}"

echo /var/$BASE_DIR > /var/SUIL_BASE
echo $DOWNLOAD_URL  > /var/SUIL_DOWNLOAD_URL
