#!/bin/bash

RETCODE=$(fw_exists /usr/bin/mvn)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get -y install maven -qq
mvn -version
