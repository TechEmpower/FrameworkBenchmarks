#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/dart-sdk.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-x64-release.zip
fw_unzip dartsdk-linux-x64-release.zip

touch ${IROOT}/dart-sdk.installed