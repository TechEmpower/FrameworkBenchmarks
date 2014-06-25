#!/bin/bash

RETCODE=$(fw_exists sbt/bin/sbt)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://dl.bintray.com/sbt/native-packages/sbt/0.13.5/sbt-0.13.5.zip -O sbt-0.13.5.zip
fw_unzip sbt-0.13.5.zip
