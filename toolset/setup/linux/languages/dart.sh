#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/dart-sdk.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/dart-sdk.installed
  return 0; }

DART_HOME=$IROOT/dart-sdk

fw_get -O http://storage.googleapis.com/dart-archive/channels/stable/release/42828/sdk/dartsdk-linux-x64-release.zip
fw_unzip dartsdk-linux-x64-release.zip

echo "export DART_HOME=${DART_HOME}" > $IROOT/dart-sdk.installed
echo "export PUB_CACHE=${IROOT}/.pubcache" >> $IROOT/dart-sdk.installed
echo -e "export PATH=\$DART_HOME/bin:\$PATH" >> $IROOT/dart-sdk.installed

source $IROOT/dart-sdk.installed
