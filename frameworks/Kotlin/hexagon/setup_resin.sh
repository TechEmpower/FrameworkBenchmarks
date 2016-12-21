#!/bin/bash

fw_depends mongodb java resin

gradle/wrapper

cp -f build/libs/ROOT.war $RESIN_HOME/webapps
resinctl start
