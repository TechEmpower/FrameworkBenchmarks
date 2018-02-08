#!/bin/bash

fw_depends gcc-6

fw_installed openssl && return 0

OPENSSL_VERSION=1.0.2n
TEMP_DIRECTORY="$(mktemp -d)"
OPENSSL_FILENAME=openssl-${OPENSSL_VERSION}
OPENSSL_URL=https://www.openssl.org/source/${OPENSSL_FILENAME}.tar.gz
OPENSSL_SIGNATURE=https://www.openssl.org/source/${OPENSSL_FILENAME}.tar.gz.asc

# Import the PGP keys of OpenSSL team. See the website for updates
gpg --recv-keys 8657ABB260F056B1E5190839D9C4D26D0E604491 5B2545DAB21995F4088CEFAA36CEE4DEB00CFE33 C1F33DD8CE1D4CC613AF14DA9195C48241FBF7DD 7953AC1FBC3DC8B3B292393ED5E9E43F7DF9EE8C E5E52560DD91C556DDBDA5D02064C53641C25E5D D099684DC7C21E02E14A8AFEF23479455C51B27C

# Download the files at temporal storage
wget -P ${TEMP_DIRECTORY} ${OPENSSL_SIGNATURE}
wget -P ${TEMP_DIRECTORY} ${OPENSSL_URL} 

# Verify the downloaded file using the signature file
gpg --verify ${TEMP_DIRECTORY}/${OPENSSL_FILENAME}.tar.gz.asc ${TEMP_DIRECTORY}/${OPENSSL_FILENAME}.tar.gz

# Compile the project
pushd "${TEMP_DIRECTORY}"
fw_untar "${TEMP_DIRECTORY}/${OPENSSL_FILENAME}.tar.gz"
pushd "$OPENSSL_FILENAME"
./config --shared
make
sudo make install
sudo ln -sf /usr/local/ssl/bin/openssl `which openssl`
openssl version -v
sudo ldconfig
popd
popd

echo "# OpenSSL should be in /usr/local/ssl" > $IROOT/openssl.installed
