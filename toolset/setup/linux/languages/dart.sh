#!/bin/bash

DART_HOME=$IROOT/dart-sdk
RETCODE=$(fw_exists ${DART_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $DART_HOME.installed
  return 0; }

fw_get http://storage.googleapis.com/dart-archive/channels/stable/release/42828/sdk/dartsdk-linux-x64-release.zip
fw_unzip dartsdk-linux-x64-release.zip

echo "export DART_HOME=${DART_HOME}" > $IROOT/dart-sdk.installed
echo "export PUB_CACHE=${IROOT}/.pubcache" >> $IROOT/dart-sdk.installed
echo -e "export PATH=${DART_HOME}/bin:\$PATH" >> $IROOT/dart-sdk.installed

source $IROOT/dart-sdk.installed
