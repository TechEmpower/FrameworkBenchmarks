#!/bin/bash

fw_depends java openssl apache-apr

fw_installed tomcat && return 0

TOMCAT_VERSION=9.0.5
TEMP_DIRECTORY="$(mktemp -d)"

# Ask Apache about the preferred mirror for our connection using JSON response. Source: https://stackoverflow.com/a/39670213
APACHE_MIRROR="$(wget -qO - http://www.apache.org/dyn/closer.lua?as_json=1 | grep -P '"preferred": "' | cut -d \" -f4)"
TOMCAT_FILENAME=apache-tomcat-${TOMCAT_VERSION}
TOMCAT_URL=${APACHE_MIRROR}tomcat/tomcat-9/v${TOMCAT_VERSION}/bin/${TOMCAT_FILENAME}.tar.gz

TOMCAT_SIGNATURE=https://www.apache.org/dist/tomcat/tomcat-9/v${TOMCAT_VERSION}/bin/apache-tomcat-${TOMCAT_VERSION}.tar.gz.asc

APACHE_KEYS=https://www.apache.org/dist/tomcat/tomcat-9/KEYS

# Download the files at temporal storage
wget -P ${TEMP_DIRECTORY} ${APACHE_KEYS}
wget -P ${TEMP_DIRECTORY} ${TOMCAT_SIGNATURE}
wget -P ${TEMP_DIRECTORY} ${TOMCAT_URL} 

# It's highly unlikely Apache to change the filename of the GPG keys file
gpg --import ${TEMP_DIRECTORY}/KEYS

# Verify the downloaded file using the signature file
gpg --verify ${TEMP_DIRECTORY}/${TOMCAT_FILENAME}.tar.gz.asc ${TEMP_DIRECTORY}/${TOMCAT_FILENAME}.tar.gz

tar -xzf ${TEMP_DIRECTORY}/${TOMCAT_FILENAME}.tar.gz -C $IROOT

# Build Tomcat Native adapter - Apache APR
pushd "${IROOT}/${TOMCAT_FILENAME}/bin"
mkdir -p tomcat-native
tar -xzf tomcat-native.tar.gz -C tomcat-native --strip-components=1
pushd "tomcat-native/native"
./configure --with-apr=/usr/local/apr --with-ssl=/usr/local/ssl
make
sudo make install
popd
popd

# Configure default server.xml
pushd "${IROOT}/${TOMCAT_FILENAME}/conf/"
mv server.xml server.xml.orig
cat $FWROOT/toolset/setup/linux/webservers/tomcat/server.xml > server.xml
popd

# Configure Tomcat to use compiled native adapter
echo -e "LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/apr/lib:/usr/local/ssl/lib\n
export LD_LIBRARY_PATH" > ${IROOT}/${TOMCAT_FILENAME}/bin/setenv.sh

echo "export CATALINA_HOME=$IROOT/$TOMCAT_FILENAME" > $IROOT/tomcat.installed
echo -e "export PATH=\$IROOT/$TOMCAT_FILENAME/bin:\$PATH" >> $IROOT/tomcat.installed

source $IROOT/tomcat.installed
