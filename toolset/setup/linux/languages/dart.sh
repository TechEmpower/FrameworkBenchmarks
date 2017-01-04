#!/bin/bash

fw_installed dart-sdk && return 0

DART_HOME=$IROOT/dart-sdk

# Dart version 1.10.0
fw_get -O http://storage.googleapis.com/dart-archive/channels/stable/release/45396/sdk/dartsdk-linux-x64-release.zip
fw_unzip dartsdk-linux-x64-release.zip

echo "export DART_HOME=${DART_HOME}" > $IROOT/dart-sdk.installed
echo "export PUB_CACHE=${IROOT}/.pubcache" >> $IROOT/dart-sdk.installed
echo -e "export PATH=\$DART_HOME/bin:\$PATH" >> $IROOT/dart-sdk.installed

source $IROOT/dart-sdk.installed
