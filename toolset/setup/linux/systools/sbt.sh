#!/bin/bash

fw_installed sbt && return 0
  
VERSION="1.0.4"

fw_get -o sbt-$VERSION.zip https://github.com/sbt/sbt/releases/download/v$VERSION/sbt-$VERSION.zip
fw_unzip sbt-$VERSION.zip

echo -e "export PATH=${IROOT}/sbt/bin:\$PATH" > $IROOT/sbt.installed

source $IROOT/sbt.installed
