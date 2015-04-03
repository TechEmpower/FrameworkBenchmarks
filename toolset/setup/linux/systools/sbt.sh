#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/sbt.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sbt_ver=0.13.8
fw_get http://dl.bintray.com/sbt/native-packages/sbt/$sbt_ver/sbt-$sbt_ver.zip -O sbt-$sbt_ver.zip
fw_unzip sbt-$sbt_ver.zip

touch ${IROOT}/sbt.installed
