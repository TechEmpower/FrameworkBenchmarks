#!/bin/bash

fw_installed sbt && return 0
  
VERSION="0.13.9"

fw_get -o sbt-$VERSION.zip http://dl.bintray.com/sbt/native-packages/sbt/$VERSION/sbt-$VERSION.zip
fw_unzip sbt-$VERSION.zip

echo -e "export PATH=${IROOT}/sbt/bin:\$PATH" > $IROOT/sbt.installed

source $IROOT/sbt.installed
