#!/bin/bash

fw_installed maven && return 0

MAVEN_VERSION=3.5.2
TEMP_DIRECTORY="$(mktemp -d)"

# Ask Apache about the preferred mirror for our connection using JSON response. Source: https://stackoverflow.com/a/39670213
MAVEN_MIRROR="$(wget -qO - http://www.apache.org/dyn/closer.lua?as_json=1 | grep -P '"preferred": "' | cut -d \" -f4)"
MAVEN_FILENAME=apache-maven-${MAVEN_VERSION}-bin
MAVEN_URL=${MAVEN_MIRROR}maven/maven-3/${MAVEN_VERSION}/binaries/${MAVEN_FILENAME}.tar.gz
MAVEN_SIGNATURE=https://www.apache.org/dist/maven/maven-3/${MAVEN_VERSION}/binaries/${MAVEN_FILENAME}.tar.gz.asc

APACHE_KEYS=https://www.apache.org/dist/maven/KEYS

# Download the files at temporal storage
wget -P ${TEMP_DIRECTORY} ${APACHE_KEYS}
wget -P ${TEMP_DIRECTORY} ${MAVEN_SIGNATURE}
wget -P ${TEMP_DIRECTORY} ${MAVEN_URL} 

# It's highly unlikely Apache to change the filename of the GPG keys file
gpg --import ${TEMP_DIRECTORY}/KEYS

# Verify the downloaded file using the signature file
gpg --verify ${TEMP_DIRECTORY}/${MAVEN_FILENAME}.tar.gz.asc ${TEMP_DIRECTORY}/${MAVEN_FILENAME}.tar.gz

# Delete and create the installation directory
sudo rm -rf /usr/share/maven3
sudo mkdir -p /usr/share/maven3
sudo tar -xzf ${TEMP_DIRECTORY}/${MAVEN_FILENAME}.tar.gz -C /usr/share/maven3 --strip-components=1

echo "export PATH=/usr/share/maven3/bin:\$PATH" > $IROOT/maven.installed

source $IROOT/maven.installed

mvn -version

# Clean up the temp files
rm -r ~/.gnupg/
rm -r ${TEMP_DIRECTORY}
