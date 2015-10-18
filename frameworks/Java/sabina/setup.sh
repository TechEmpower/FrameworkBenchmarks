#!/bin/bash

fw_depends java8 maven

gradle/wrapper clean distZip -Ddb.host=${DBHOST}
unzip -d build/ build/distributions/sabina-1.0.0.zip
build/sabina-1.0.0/bin/sabina &

