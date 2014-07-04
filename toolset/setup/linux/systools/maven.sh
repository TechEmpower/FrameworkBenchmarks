#!/bin/bash

fw_exists /usr/bin/mvn
[ $? -ne 0 ] || { return 0; }

sudo apt-get -y install maven -qq
mvn -version
