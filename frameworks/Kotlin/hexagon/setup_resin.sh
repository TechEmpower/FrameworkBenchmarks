#!/bin/bash

fw_depends mongodb java resin

gradle/wrapper

rm -rf $RESIN_HOME/webapps/*
cp build/libs/ROOT.war $RESIN_HOME/webapps
resinctl start
