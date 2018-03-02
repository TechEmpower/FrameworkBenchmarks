#!/bin/bash

fw_depends gcc-6 openssl

fw_installed apache-apr && return 0

APR_VERSION=1.6.3
TEMP_DIRECTORY="$(mktemp -d)"

# Ask Apache about the preferred mirror for our connection using JSON response. Source: https://stackoverflow.com/a/39670213
APR_MIRROR="$(wget -qO - http://www.apache.org/dyn/closer.lua?as_json=1 | grep -P '"preferred": "' | cut -d \" -f4)"
APR_FILENAME=apr-${APR_VERSION}
APR_URL=${APR_MIRROR}apr/${APR_FILENAME}.tar.gz
APR_SIGNATURE=http://www.apache.org/dist/apr/${APR_FILENAME}.tar.gz.asc

APACHE_KEYS=https://people.apache.org/keys/group/apr.asc

# Download the files at temporal storage
wget -P ${TEMP_DIRECTORY} ${APACHE_KEYS}
wget -P ${TEMP_DIRECTORY} ${APR_SIGNATURE}
wget -P ${TEMP_DIRECTORY} ${APR_URL} 

# It's highly unlikely Apache to change the filename of the GPG keys file
gpg --import ${TEMP_DIRECTORY}/apr.asc

# Verify the downloaded file using the signature file
gpg --verify ${TEMP_DIRECTORY}/${APR_FILENAME}.tar.gz.asc ${TEMP_DIRECTORY}/${APR_FILENAME}.tar.gz

#TODO below
# Compile the project
pushd "${TEMP_DIRECTORY}"
fw_untar "${TEMP_DIRECTORY}/${APR_FILENAME}.tar.gz"
pushd "$APR_FILENAME"
# We need the newer custom OpenSSL
./configure --with-ssl=/usr/local/ssl
make
sudo make install
sudo ldconfig
popd
popd

echo "# Apache APR should be in /usr/local/ssl" > $IROOT/apache-apr.installed
